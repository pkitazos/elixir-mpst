# Elixir-Maty

A tool for statically checking Elixir communication patterns. Multiparty Session Typing for Elixir apps using the Maty language design

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `maty` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:maty, "~> 0.7.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/maty>.


## Dev things

### How to run Scribble

To check that a global protocol is correct:
```
scribble path/to/protocol.scr
```


To get a local projection of a global protocol:
```
scribble path/to/protocol.scr -project GlobalProtocolName LocalRoleName
```