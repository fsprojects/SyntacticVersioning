namespace SyntacticVersioning
type Version = Major | Minor | Patch
type NetType =
  | Abstract | Class | Enum | Interface | Static | Struct
  (* | Primitive | ValueType *)
  | Module | RecordType | SumType | UnionConstructor | UnionTags

type InstanceOrStatic = | Instance | Static
type Name = string
type Attribute = string
type Type = string
type Parameter = { Type:Type; Name:Name }
type Member= 
    |RecordConstructor of Attribute list * Parameter list
    |Constructor of Attribute list * Parameter list
    |Event of InstanceOrStatic * Name * Attribute list *Parameter list * Type
    |Field of InstanceOrStatic * Name * Type
    |Method of InstanceOrStatic * Name * Attribute list *Parameter list * Type
    |Property of InstanceOrStatic * Name * Type

type SurfaceOfType =
    { 
        NetType: NetType
        Members: Member list
    }

type Namespace=
    {
        /// A map from typename to Surface of Type
        Types: Map<string,SurfaceOfType>
    }
type Package=
    {
        Namespaces: Map<string, Namespace>
    }
