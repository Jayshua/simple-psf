#![no_std]
#![doc = include_str!("../README.md")]


/**
A PSF font

Use [parse](Psf::parse) to create one, [get_glyph_pixels](Psf::get_glyph_pixels) to get a glyph by index,
and [iter_unicode_entries](Psf::iter_unicode_entries) to get a glyph index by unicode character.
PSF stores ASCII glyphs at the corresponding index, so you can retrieve ASCII characters without calling
[iter_unicode_entries](Psf::iter_unicode_entries) by giving the ASCII number to [get_glyph_pixels](Psf::get_glyph_pixels) directly.
*/
pub struct Psf<'a> {
	pub glyph_byte_length: usize,
	pub glyph_width: usize,
	pub glyph_height: usize,
	pub glyphs: &'a [u8],
	pub unicode_table: Option<&'a [u8]>,
}

impl<'a> Psf<'a> {
	/**
	Parse a PSF font from some bytes

	See [ParseError] for possible errors.

	PSF fonts may optionally have a unicode table indicating which glyphs correspond to which
	unicode characters. This function does not validate the structure of the unicode table.
	Errors in the encoding of the unicode table, if they can be detected, are reported by the
	iterator returned from the [iter_unicode_entries](Psf::iter_unicode_entries) function.
	*/
	pub fn parse(data: &[u8]) -> Result<Psf, ParseError> {
		if data.len() < 32 {
			return Err(ParseError::HeaderMissing);
		}

		if &data[0..4] == &[0x72, 0xb5, 0x4a, 0x86] {
			Self::parse_psf2(data)
		} else if &data[0..2] == &[0x36, 0x04] {
			Self::parse_psf1(data)
		} else {
			Err(ParseError::InvalidMagicBytes)
		}
	}

	fn parse_psf1(data: &[u8]) -> Result<Psf, ParseError> {
		let mode = data[2];
		let has_512_glyphs = 0 != (mode & 0x1);
		let has_unicode_table = 0 != (mode & 0x2);

		let glyph_count =
			if has_512_glyphs {
				256
			} else {
				512
			};

		let glyph_height = data[3] as usize;

		let psf =
			Psf {
				glyph_byte_length: glyph_height,
				glyph_width: 8,
				glyph_height: glyph_height,
				glyphs: &data[4..][..glyph_count * glyph_height],
				unicode_table: None,
			};

		Ok(psf)
	}

	fn parse_psf2(data: &[u8]) -> Result<Psf, ParseError> {
		let version = u32::from_le_bytes(data[4..8].try_into().unwrap());
		if 0 != version {
			return Err(ParseError::UnknownVersion(version));
		}

		let header_size = u32::from_le_bytes(data[8..12].try_into().unwrap()) as usize;
		let flags = u32::from_le_bytes(data[12..16].try_into().unwrap());
		let has_unicode_table = 1 == (flags & 0x00000001);
		let glyph_count = u32::from_le_bytes(data[16..20].try_into().unwrap()) as usize;
		let glyph_byte_length = u32::from_le_bytes(data[20..24].try_into().unwrap()) as usize;
		let glyph_height = u32::from_le_bytes(data[24..28].try_into().unwrap()) as usize;
		let glyph_width = u32::from_le_bytes(data[28..32].try_into().unwrap()) as usize;

		let expected_byte_count = header_size + (glyph_count * glyph_byte_length);
		if data.len() < expected_byte_count {
			return Err(ParseError::GlyphTableTruncated { expected_byte_count: expected_byte_count });
		}

		let glyphs = &data[header_size..][..glyph_byte_length * glyph_count];

		let unicode_table =
			if has_unicode_table {
				Some(&data[expected_byte_count..])
			} else {
				None
			};

		let psf = Psf {
			glyph_byte_length: glyph_byte_length,
			glyph_width: glyph_width,
			glyph_height: glyph_height,
			glyphs,
			unicode_table,
		};

		Ok(psf)
	}

	/// Get the bits corresponding to the glyph's bitmap
	///
	/// PSF stores the bitmap as packed bits. Each byte in the slice contains
	/// eight pixels, with the last byte of each row containing padding bits
	/// so that the next row starts on a byte boundary.
	///
	/// You might be looking for [get_glyph_pixels](Psf::get_glyph_pixels) instead, which unpacks
	/// the bits for you.
	pub fn get_glyph_bits(&self, glyph_index: usize) -> Option<&[u8]> {
		let start = self.glyph_byte_length * glyph_index;
		let end = start + self.glyph_byte_length;
		self.glyphs.get(start..end)
	}

	/**
	Get the pixels that correspond to the given glyph's bitmap

	PSF stores bitmaps in the packed bits of each byte.
	This iterator unpacks the bits, returning a boolean for each pixel in the bitmap indicating
	whether that pixel is lit or not.

	PSF stores ASCII glyphs in the corresponding index, so you can retrieve ASCII glyphs directly
	by calling `get_glyph_pixels(b'a' as usize)`. Unicode characters must be looked up with
	(iter_unicode_entries)[Psf::iter_unicode_entries].
	*/
	pub fn get_glyph_pixels<'b>(&'b self, glyph_index: usize) -> Option<impl Iterator<Item=bool> + 'b> {
		let glyph_bits = self.get_glyph_bits(glyph_index)?;
		let bytes_per_row = (self.glyph_width + 7) / 8;

		let iterator =
			(0..self.glyph_height).flat_map(move |y| {
				(0..self.glyph_width).map(move |x| {
					let byte_index = (x / 8) + (y * bytes_per_row);
					let byte = glyph_bits[byte_index];
					let bit_offset = 7 - (x % 8);
					let bit = (byte >> bit_offset) & 1;
					bit == 1
				})
			});

		Some(iterator)
	}

	/**
	Iterate over the entries in the unicode table

	PSF fonts may optionally include a unicode table indicating which unicode character each glyph corresponds to.
	If such a table exists, this function returns an iterator that will yield each unicode entry along with the
	index of the glyph that represents to it.

	In the case of a malformed unicode table, the behavior of this iterator is unpredictable
	because errors can not always be unambiguously detected. The iterator may return a unicode entry with
	a string containing multiple graphemes that ought to each have their own glyph, it may return None,
	or it may return a [core::str::Utf8Error].

	If you are using the standard library, this iterator can be used to construct a HashMap lookup table of unicode
	character to glyph index pairs.

	```rust(ignore)
	let glyph_lookup_table = psf.iter_unicode_entries().unwrap()
		.filter_map(|(index, result)| result.ok().map(|character| (character, index)))
		.collect::<std::collections::HashMap<&str, usize>>();
	```
	*/
	pub fn iter_unicode_entries<'b>(&'b self) -> Option<impl Iterator<Item=(usize, Result<&'b str, core::str::Utf8Error>)> + 'b> {
		let table = self.unicode_table?;

		let iterator = table.split(|&x| x == 0xff).enumerate().flat_map(move |(glyph_index, unicode_entries)| {
			unicode_entries.split(|&x| x == 0xfe).map(move |unicode_string| {
				(glyph_index, core::str::from_utf8(unicode_string))
			})
		});

		Some(iterator)
	}
}


/// Possible errors returned by the [Psf::parse] function.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ParseError {
	/// The provided buffer is not large enough to contain the PSF header.
	HeaderMissing,

	/// The PSF header does not contain 72 b5 4a 86 as its first four bytes.
	InvalidMagicBytes,

	/// The PSF header contains a version other than 0.
	/// (0 is the only version that exists as of writing)
	UnknownVersion(u32),

	/// The provided buffer is not large enough to contain all of the glyphs
	/// that the PSF header indicated that it should.
	GlyphTableTruncated { expected_byte_count: usize, },
}
