# Project 3

By: Jonathan Bravo UFID: 11869914 & Yu Hong UFID: 35156371

## Usage

### Building

First you must `cd` into the project directory to compile the code. This can be
done two ways. Either in a single line using:

```erlang
erlc project3.erl
```

or inside of an erlang vm using `erl` and then once the vm is active typing:

```erlang
c(project3).
```

### Running Code

Code can be run by typing this format
`project3:start(numNodes, NumRequests).` into an erlang shell.

Ex:

```erlang
project3:start(10, 1).
```

## What is working

We were able to get the standard functionality into the code. There is a chord
ring of size $2^M$ that contains the number of nodes requested and each node
will make the given number of requests. A user can see the average number of
jumps each nodes takes by typing:

```erlang
chord_ring ! display_jump_average.
```

A user may also see any individual nodes id and finger table by typing:

```erlang
Pid ! print_self.
```

The above is an example and the pid will change depending on the number of nodes
that are spawned. The node list is printed to the console when the `start`
command is run. The pid should be entered as follows `<0.100.0>`.

Additional nodes can be spawned by typing:

```erlang
chord_ring ! {spawn_nodes, NumNodes, M}.
```

The desiered number of nodes to add to the network and the value M, which we
dynamically selected by doing some math based on the original number of nodes:

```erlang
M = erlang:trunc(math:ceil(math:sqrt(NumNodes))) + 2
```

Getting the value of `M` in this way ensured we could handle a larger network.
If a user is going to spawn more nodes they need to ensure they enter the
correct value of `M` so they do not get an error.

## Largest Network

We were able to handle a network of 100 nodes.

## Additional Comments

The `start` function contains a list of "files" that can be updated. Requests
from each node only chose from the list of files. We could have extended this by
returning somthing like "The file does not exists" when a node made a request
for a file who did not exist on the ring.

The `chord_ring` is actually its own process that exists to resolve the pid
when building/ updating the individual nodes finger tables and maintain the
node list of the ring itself.