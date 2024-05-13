use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use syn::{parse_macro_input, spanned::Spanned, Data, DeriveInput, Field, Type};

#[proc_macro_derive(Trace)]
pub fn derive_tracable(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let body = generate_trace_body(&input.data);

    let expanded = quote!(
        unsafe impl #impl_generics gc::Trace for #name #ty_generics #where_clause {
            fn trace(&mut self, trace_fn: gc::TraceCallback) {
                #body
            }
        }
    );

    proc_macro::TokenStream::from(expanded)
}

fn generate_field_access(idx: usize, field: &Field) -> proc_macro2::TokenStream {
    let accessor = field
        .ident
        .as_ref()
        .map(|i| i.into_token_stream())
        .unwrap_or(idx.into_token_stream());
    match &field.ty {
        Type::Array(_) => todo!("a"),
        Type::Group(_) => todo!("group"),
        Type::Infer(_) => todo!("infer"),
        Type::Macro(_) => panic!("macro in type"),
        Type::Paren(_) => todo!("paren"),
        Type::Path(p) => {
            let name = p
                .path
                .segments
                .last()
                .expect("expecting at least one path segment entry");

            let ident = &name.ident;
            if ident == "RawGcPointer" {
                quote_spanned! {field.span() => trace_fn(&mut self.#accessor);}
            } else if ident == "GcPointer" {
                quote_spanned! {field.span() => trace_fn(self.#accessor.as_mut());}
            } else {
                quote!()
            }
        }
        Type::Reference(_) => todo!("reference"),
        Type::Slice(_) => todo!("slice"),
        Type::Tuple(_) => todo!("tuple"),
        Type::Verbatim(_) => todo!("verbatim"),
        Type::BareFn(_)
        | Type::ImplTrait(_)
        | Type::Never(_)
        | Type::Ptr(_)
        | Type::TraitObject(_) => quote!(),
        _ => unimplemented!("not available at time of crate authoring"),
    }
}

fn generate_trace_body(data: &Data) -> proc_macro2::TokenStream {
    match data {
        Data::Struct(s) => {
            let fields = s
                .fields
                .iter()
                .enumerate()
                .map(|(idx, f)| generate_field_access(idx, f));
            quote!(#(#fields)*)
        }
        Data::Enum(_) => todo!(),
        Data::Union(_) => {
            panic!("C unions are not supported as there is no indication which variant is correct.")
        }
    }
}
