namespace SpiseMisu

type MyClassWithCLIEvent() =
  let event1 = new Event<_>()

  [<CLIEvent>]
  member this.Event1 = event1.Publish
  member this.TestEvent(arg) =
    event1.Trigger(this, arg)

type 'a Record = { foo: int; bar: float; fooBar: 'a -> 'a -> 'a -> 'a } with
  static member StaticFoo = 42
  member x.MemberBar = 42.

type Union = FooBar | Foo | Bar 

type UnionPrime = Foo of (int * float) | Bar of float with
  member x.Baz = 42
  static member Qux () = 42


type Enum = | Foo = 0 | Bar = 42

[<Struct>]
type Struct(foo:int,bar:float) =
  member x.Foo = foo
  member x.Bar = bar

type Type (number:int,str:string) =
  // let constant = 42
  //member x.Member () = 42
  let constant1 = number
  let constant2 = str
  static member StaticMember () = 42

module Module =
  open System
  // let constant = 42
  
  let add x y z = x + y + z
  
  let add' (x,y) z (a,b,c) = x + y + z + a + b + c
  
  let lambda = fun () -> [[((),())]]

  let dict = Map.empty |> Map.add "42" 42

  let generic : 'a list -> ('a -> 'b) -> 'b list =
    fun xs f -> xs |> List.map f

  let identity : 'a -> 'a = fun x -> x
  
  // let lambda = fun x y -> x + y
  
  // let lambda' = fun (x,y) -> x + y
