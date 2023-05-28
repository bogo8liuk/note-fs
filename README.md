# note-fs
A simple command-line tool to take notes on the file-system.

### Commands
There is a bunch of useful commands:
- `nfs-see`, to watch the notes taken on a file
- `nfs-take`, to take notes on a file
- `nfs-edit`, to change the notes of a file

### Repl
There is also a repl (`nfs-repl` command) which have all the commands in Commands section with a more pleasant syntax.
Moreover, the repl is lazy by default, in the sense that the changes made by the user aren't immediately committed to the
file-system, but they are maintained in a cache. When the user exits the repl or runs the `commit` command, the changes
are written to the file system. This is done for a better efficiency, but it is recommended not to have multiple sessions
of the repl, since it can bring to (deliberately unhandled) race conditions.

### Dependencies
`note-fs` has some dependencies:
- Haskell language, since the tool is actually written in Haskell;
- stack build system;
- `hs-utils` library, available (here)[https://github.com/bogo8liuk/hs-utils];

### Contributing
Contributions are welcomed, so feel free to start a pull request or to open an issue! Anyway, if you want to implement
a new command, you can follow these quite simple procedures:
1. Add a new command op-code in the type `Command`, in `src/Commands.hs` source;
2. Add the new lexical tokens in `src/Commands/Lexer.hs` source, one for the executable command and one for the repl
command;
3. Implement a parser in `src/Commands/Parsing.hs` source for your command, see the precise guidelines directly in the
source;
4. Write an implementation of your command in `src/Commands/Impl.hs` and update the `runCommand` function. For the
functionalities you want to implement, you can exploit facilities in `src/Env.hs`;
5. (Optional) If you want to have a new executable, add a new function in `src/Programs.hs` with the name of your
command. As implementation, it should be enough evaluating the `execCommand` function. Then:
    1. Create a new file in the `app` folder, `import Programs` and write a `main` where you evaluate your command
    function;
    2. Update `package.yaml`, adding the executable in the `executables` section. Be aware that the name of the
    executable MUST be equal to the name of executable you gave in the lexer (this is quite bad, so this is a TODO).

### License
GPL-3