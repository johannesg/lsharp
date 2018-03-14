module LSharp.Core

open System.Collections.Immutable

type Form = 
| Empty
| String of string
| Symbol of string
| Keyword of string
| Number of int
| Quote of Form
| List of Form list
| Vector of Form array
| Map of ImmutableDictionary<Form, Form>
| Set of ImmutableHashSet<Form>
| Object of obj

type Fn = {
    Invoke : Form list -> Result<Form, string>
}
