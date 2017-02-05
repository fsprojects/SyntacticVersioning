namespace SyntacticVersioning
type Version = Major | Minor | Patch
type NetType =
  | Abstract | Class | Enum | Interface | Static | Struct
  (* | Primitive | ValueType *)
  | Module | RecordType | SumType | UnionConstructor | UnionTags

type InstanceOrStatic = | Instance | Static
type Name = string
type AttributeTypedArgument ={Value:obj}
type Attribute = {FullName:string; ConstructorArguments: AttributeTypedArgument list}
type Typ = { FullName:string }
type Parameter = { Type:Typ; Name:Name }

type Constructor=Typ *Attribute list * Parameter list

type Member= 
    |RecordConstructor of Constructor
    |Constructor of Constructor
    |Event of Typ * InstanceOrStatic * Name * Attribute list *Parameter list * Typ
    |Field of Typ *InstanceOrStatic * Name * Typ
    |Method of Typ *InstanceOrStatic * Name * Attribute list *Parameter list * Typ
    |Property of Typ *InstanceOrStatic * Name * Typ
    |UnionConstructors of Typ*(Constructor list)
(*    with
        override m.ToString ()=
            match m with
            | RecordConstructor (t,attrs,parameters) -> sprintf "{ %s.%s } -> %s" t.FullName (Print.parameters (attrs,parameters)) t.FullName
*)



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
