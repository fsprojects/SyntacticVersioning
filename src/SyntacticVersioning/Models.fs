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
type Type = { FullName:string }
type Parameter = { Type:Type; Name:Name }

module internal Print =
  let whenStatic (flag:bool) (fulltypename:string) : string =
        match flag with
          | true -> fulltypename
          | false -> sprintf "(Instance/Inheritance of %s)" fulltypename
  let parameters (attrs,parameters)=failwith "not implemented"

type Member= 
    |RecordConstructor of Type * Attribute list * Parameter list
    |Constructor of Type *Attribute list * Parameter list
    |Event of Type * InstanceOrStatic * Name * Attribute list *Parameter list * Type
    |Field of Type *InstanceOrStatic * Name * Type
    |Method of Type *InstanceOrStatic * Name * Attribute list *Parameter list * Type
    |Property of Type *InstanceOrStatic * Name * Type
    with
        override m.ToString ()=
            match m with
            | RecordConstructor (t,attrs,parameters) -> sprintf "{ %s.%s } -> %s" t.FullName (Print.parameters (attrs,parameters)) t.FullName

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
