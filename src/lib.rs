//! Flatten enum into struct.
//! ```
//!# use serde_derive::{Serialize, Deserialize};
//! #[derive(Debug, Serialize, Deserialize, PartialEq, Eq)]
//! pub struct TextElement {
//!     text: String,
//!     font_size: u32
//! }
//!
//! #[derive(Debug, Serialize, Deserialize, PartialEq, Eq)]
//! pub struct ImageElement {
//!     bitmap: Vec<[u8; 4]>
//! }
//! 
//! #[derive(Debug, Serialize, Deserialize, PartialEq, Eq)]
//! pub enum ElementInner {
//!     Text(TextElement),
//!     Image(ImageElement)
//! }
//!
//! serde_flatten_enum::flatten_enum! {
//!     #[passthru(derive(Debug, PartialEq, Eq))]
//!     #[flatten_enum(serde)]
//!     #[flatten_enum(derive(Serialize, Deserialize))]
//!     pub struct Element {
//!         inner: ElementInner {
//!             Text(TextElement),
//!             Image(ImageElement)
//!         },
//!         width: u32,
//!         height: u32
//!     }
//! }
//!
//! fn main() {
//!     let element = Element {
//!         inner: ElementInner::Text(TextElement {
//!             text: "Hello, world!".to_string(),
//!             font_size: 16
//!         }),
//!         width: 200,
//!         height: 20
//!     };
//!     let json = r#"{"Text":{"text":"Hello, world!","font_size":16,"width":200,"height":20}}"#;
//!     assert_eq!(serde_json::to_string(&element).expect("serialization failed"), json);
//!     assert_eq!(serde_json::from_str::<'_, Element>(json).expect("deserialization failed"), element);
//! }
//! ```
//! How to use:
//! - You need to use a patched version of Serde.
//! - The struct must be defined inside the macro.
//! - The enum must be defined outside the macro.
//! This does mean duplicating all the variant names, but
//! doing so allows wrapping enums outside of your crate.
//! - The topmost field must be an enum to be flattened.
//! - All variants of the enum must be a struct newtype.
//! - All variants of the enum must be named inline.
//! Each variant's struct name have to be named too.
//! However, if the struct and the variant have the same name,
//! the struct name can be omitted.
//! - Any attributes that affect serializing/deserializing must be
//! wrapped like `#[flatten_enum(serde_attribute)]`.
//! - The first `#[flatten_enum]` attribute should contain literal `serde`.
//! If your struct only need either `Serialize` or `Deserialize`,
//! write respectively `ser` or `de` instead.
//! - Any other attributes must be wrapped like `#[passthru(other_attribute)]`.
//! - All `#[flatten_enum]` attributes must be specified after
//! `#[passthru]` attrubutes.
//! - Manual implementations of `Serialize`/`Deserialize`
//! can be passed as second argument to the macro.
//! - `#[serde(flatten)]` attribute is automatically specified,
//! so you don't have to write it yourself.
//! - `#[serde(rename)]` and `#[serde(rename_all)]` is not supported.

// Pass identifiers within macros to bypass hygiene.
#[macro_export]
macro_rules! flatten_enum {
	// Entry point is duplicated because `$ftype` cannot be passed
	// into another macro arm.
	(
		$(#[passthru($ptm:meta)])*
		#[flatten_enum($serde:ident)]
		$(#[flatten_enum($sfem:meta)])*
		$vis:vis struct $struct:ident {
			$(#[passthru($ptmi:meta)])* $(#[flatten_enum($sfemi:meta)])*
			$inner:ident: $($enum:ident)::* { $($variant:tt),* },
			$(
				$(#[passthru($ptmf:meta)])* $(#[flatten_enum($sfemf:meta)])*
				$fname:ident: $ftype:ty
			),*
		}
		$(,$($impl:item)+)?
		
	) => {

		$(#[$ptm])* $vis struct $struct {
			$(#[$ptmi])* $inner: $($enum)::*,
			$($(#[$ptmf])* $fname: $ftype),*
		}
		const _: () = {
			$(#[$sfem])* struct Typed<T> {
				$(#[$sfemi])* #[serde(flatten)] $inner: T,
				$($(#[$sfemf])* $fname: $ftype),*
			}

			$(,$($impl)+)?

			use ::std::{fmt, result::Result::{self, Ok}};

			$crate::flatten_enum!(
				@$serde $struct { $($fname),* }, $inner, $($enum)::* { $($variant($variant)),* }
			);

			$crate::flatten_enum!(
				@$serde typed, variant, { $inner: variant, $($fname: typed.$fname),* },
				$struct, $inner, $($enum)::* { $($variant($variant)),* }
			);
		};
	};

	(
		$(#[passthru($ptm:meta)])*
		#[flatten_enum($serde:ident)]
		$(#[flatten_enum($sfem:meta)])*
		$vis:vis struct $struct:ident {
			$(#[passthru($ptmi:meta)])* $(#[flatten_enum($sfemi:meta)])*
			$inner:ident: $($enum:ident)::* { $($vname:ident $vtype:tt),* },
			$(
				$(#[passthru($ptmf:meta)])* $(#[flatten_enum($sfemf:meta)])*
				$fname:ident: $ftype:ty
			),*
		}
		$(,$($impl:item)+)?

	) => {

		$(#[$ptm])* $vis struct $struct {
			$(#[$ptmi])* $inner: $($enum)::*,
			$($(#[$ptmf])* $fname: $ftype),*
		}
		const _: () = {
			$(#[$sfem])* struct Typed<T> {
				$(#[$sfemi])* #[serde(flatten)] $inner: T,
				$($(#[$sfemf])* $fname: $ftype),*
			}

			$(,$($impl)+)?

			use ::std::{fmt, result::Result::{self, Ok}};

			$crate::flatten_enum!(
				@$serde $struct { $($fname),* }, $inner, $($enum)::* { $($vname $vtype),* }
			);

			$crate::flatten_enum!(
				@$serde typed, variant, { $inner: variant, $($fname: typed.$fname),* },
				$struct, $inner, $($enum)::* { $($vname $vtype),* }
			);
		};
	};

	// Translate `@serde` into `@ser` and `@de`.
	(
		@serde $struct:ident { $($fname:ident),* }, $inner:ident,
		$($enum:ident)::* { $($vname:ident $vtype:tt),* }
	) => {
		$crate::flatten_enum!(
			@ser $struct { $($fname),* }, $inner, $($enum)::* { $($vname $vtype),* }
		)
	};

	(
		@serde $typed:ident, $variant:ident, $constructor:tt,
		$struct:ident, $inner:ident, $($enum:ident)::* { $($vname:ident $vtype:tt),* }
	) => {
		$crate::flatten_enum!(
			@de $typed, $variant, $constructor,
			$struct, $inner, $($enum)::* { $($vname $vtype),* }
		);
	};

	// Consume `@ser` input in `@de` context and vice versa.
	(
		@ser $typed:ident, $variant:ident, $constructor:tt,
		$struct:ident, $inner:ident, $enum:ty { $($vname:ident($vtype:ty)),* }
	) => {};
	
	(
		@de $struct:ident { $($fname:ident),* }, $inner:ident,
		$enum:ty { $($vname:ident($vtype:ty)),* }
	) => {};

	// Use recursive macro to generate the variant index.
	// Use wrapper type to work around trivial bounds limitation.
	(
		@ser $struct:ident { $($fname:ident),* }, $inner:ident,
		$($enum:ident)::* { $($vname:ident $vtype:tt),* }
	) => {
		use ::serde::{Serialize, ser::Serializer};
		impl<'a> Serialize for $struct {
			fn serialize<S: Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
				$crate::flatten_enum!(
					@ser self, ser, $struct { $($fname),* }, $inner,
					$($enum)::* { $($vname),* }, 0
				);
			}
		}
	};

	// Use `if let` instead of `match` to work around
	// inability of macros to emit individual match arms.
	(
		@ser $self:ident, $ser:ident, $struct:ident { $($fname:ident),* }, $inner:ident,
		$($enum:ident)::* { $name:ident $(,$rest:ident)* }, $i:tt
	) => {
		if let $($enum)::*::$name(v) = &$self.$inner {
			return $ser.serialize_newtype_variant(stringify!($struct), $i, stringify!($name), &Typed {
				inner: v,
				$($fname: $self.$fname),*
			});
		}
		$crate::flatten_enum!(
			@ser $self, $ser, $struct { $($fname),* }, $inner,
			$($enum)::* { $($rest),* }, ($i + 1)
		);
	};

	// All variants have been matched at this point,
	// so the tail case is unreachable.
	(
		@ser $self:ident, $ser:ident, $struct:ident { $($fname:ident),* },
		$inner:ident, $enum:ty {}, $i:expr
	) => { unreachable!() };

	// The constructor is split into parent rules to bypass nested repetition rules.
	(
		@de $typed:ident, $variant:ident, $constructor:tt,
		$struct:ident, $inner:ident, $enum:ty { $($vname:ident($vtype:ty)),* }
	) => {
		use ::serde::{Deserialize, de::{Deserializer, EnumAccess, VariantAccess, Visitor}};

		#[derive(::serde_derive::Deserialize)] enum Kind { $($vname),* }

		impl <'de> Deserialize<'de> for $struct {
			fn deserialize<D: Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
				de.deserialize_enum(stringify!($struct), &[$(stringify!($vname)),*], EnumVisitor)
			}
		}

		struct EnumVisitor;

		impl <'de> Visitor<'de> for EnumVisitor {
			type Value = $struct;

			fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
				f.write_str("an enum variant")
			}

			fn visit_enum<A: EnumAccess<'de>>(self, data: A) -> Result<Self::Value, A::Error> {
				match data.variant::<Kind>()? {
					$((Kind::$vname, v) => {
						let $typed = VariantAccess::newtype_variant::<Typed<$vtype>>(v)?;
						let $variant = <$enum>::$vname($typed.$inner);
						Ok($struct $constructor)
					}),*
				}
			}
		}
	}
}