mod module;
mod define;
mod import;
mod function;
mod variant;
mod interface;
mod r#struct;
mod builtin;
mod external;
mod module_path;
mod method;
mod typed_identifier;
mod type_var;

pub type Module             = module::T;
pub type Define             = define::T;
pub type Import             = import::T;
pub type Function           = function::T;
pub type Variant            = variant::T;
pub type VariantCase        = variant::Case;
pub type Interface          = interface::T;
pub type InterfaceSignature = interface::Signature;
pub type Struct             = r#struct::T;
pub type BuiltIn            = builtin::T;
pub type External           = external::T;
pub type ModulePath         = module_path::T;
pub type Method             = method::T;
pub type MethodSignature    = method::Signature;
pub type MethodConstraint   = method::Constraint;
pub type TypedIdentifier    = typed_identifier::T;
pub type TypeVar            = type_var::T;

pub enum Declaration {
    ModulePath(ModulePath),
    Define(Define),
    Import(Import),
    Function(Function),
    Variant(Variant),
    Interface(Interface),
    Struct(Struct),
    BuiltIn(BuiltIn),
    External(External),
}
