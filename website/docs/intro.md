---
sidebar_position: 1
---

## Installation

## Your first component


## Integrating with existing components

```fsharp
react {
  let! props = Props
  do! Render(
    Html.div [
      prop.text props.message
    ]
  )
}
```

## Control Flow


## Operations

Operations handle a number of roles, including 

## Rendering 

## Handling events

## Abstracting parts of a sequence 

### Returning a value from 

## Wait2, WaitAny2


## Timeout, Intervals and Counter.

## Memoization and explicit disposal

Operations are disposed by default when hacn re-executes from an earlier step. To handle cases where you want to explicitly 

Disposal control is related to Memoization, since a 

The `Once` operation 

This mean

To hand

## Calling a function passed in from a parent component