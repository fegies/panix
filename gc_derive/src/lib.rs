use quote::{quote, quote_spanned, ToTokens};
use syn::{parse_macro_input, spanned::Spanned, Data, DeriveInput, Field, Variant};

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

fn generate_field_access(
    idx: usize,
    field: &Field,
    parent_expr: &proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let accessor = field
        .ident
        .as_ref()
        .map(|i| i.into_token_stream())
        .unwrap_or(syn::Index::from(idx).into_token_stream());
    quote_spanned! {field.span() => ::gc::Trace::trace(#parent_expr.#accessor, trace_fn);}
}

fn generate_variant_access(variant: &Variant) -> proc_macro2::TokenStream {
    let ident = &variant.ident;
    let mut_t = quote!(&mut);

    if variant.fields.len() == 0 {
        quote!(Self::#ident => {})
    } else {
        todo!()
    }
}

fn generate_trace_body(data: &Data) -> proc_macro2::TokenStream {
    match data {
        Data::Struct(s) => {
            let self_expr = quote!(&mut self);
            let fields = s
                .fields
                .iter()
                .enumerate()
                .map(|(idx, f)| generate_field_access(idx, f, &self_expr));
            quote!(#(#fields)*)
        }
        Data::Enum(e) => {
            let variants = e.variants.iter().map(generate_variant_access);
            let variants = quote!(#(#variants)*);
            quote!(match self {
                #variants
            })
        }
        Data::Union(_) => {
            panic!("C unions are not supported as there is no indication which variant is correct.")
        }
    }
}
