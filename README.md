A simple parser for the [PC Screen Font](https://en.wikipedia.org/wiki/PC_Screen_Font) file format used to store bitmap fonts, most notably
for use in the Linux kernel's built-in console.

[Cozette](https://github.com/slavfox/Cozette) is a good sample PCF font if you're looking for one.

This crate is no_std, no_alloc, and should never panic.

# Example

```rust
let data: &[u8] = &std::fs::read("cozette.psf").unwrap();
let pcf = simple_pcf::Pcf::parse(data).unwrap();

for glyph_index in b'a' as usize ..= b'z' as usize {
	for (index, pixel_on) in pcf.get_glyph_pixels(glyph_index).unwrap().enumerate() {
		if index % pcf.glyph_width == 0 {
			println!("");
		}

		if pixel_on {
			print!("@");
		} else {
			print!(" ");
		}
	}
}

```