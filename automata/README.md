# automata
## application
Run the application via `stack run`.
There are three modes in the app: `(app)`, `(editing ...)`, and `(running ...)`.
1. In the `(app)` commands for basic operations like storing and loading files are supported.
2. In the editing mode you can change the automaton already loaded into memory or create a new one.
3. In the running mode you can simulate running automatons with different inputs. 
## load automata
New automata can be created via `new` command (you will enter the editing mode) or loaded from the file (in that case they  must be loaded to the root of the project with extension `.automaton.txt`).
The format of the file is simply a sequence of several statements divided by the separator `;`.
You can check the sample `acceptFinal0.automaton.txt`.
## report
1. I tried to play with finite automata: mostly parsing and constructing them, but also running. A finite automaton has a set of possible _states_ and _rules_ (they're also called _transitions_ in my project) it uses to change the state when consuming the next character in the input. It also has one _initial_ state chosen from its states set and a subset of _final_ states. If the automaton stops in one of them after consuming the whole string, the string is regarded as accepted by the automaton and belongs to the _language_ defined by it. I learned to parse the description of the automaton, construct them via command line application and run them in the same application. 
2-4. My project consists of three large subapps, the api for automaton descriptions and parser for them. The api is basically a wrapper over `Data.Set` and `Data.Map` which incapsulates the logic for modifying the automaton description in order not to lose important invariants. For a parser a library `Parsec` was chosen. This decision was made because I already played with self-written parsers enough and it was interseting to watch at the actual library. `Parsec` seems to be a gold standard of parser libraries and is also accessible on stackage ehich is the reason I chose it explicitly. It seems like a bit of an overkill though for such a simple language as mine. Nevertheless, I don't think I could support `Parsec`'s level of error reporting well in time. Subapps are represented as `State` monad transformers with the `IO` as an underlying monad, but with different states. That way I could use a similar methods for all three of them which I gladly did. Ideally the logic for the `Runner` must not be included in the `IO` methods (and be a separate entity similar to the description API), but it wasn't a large piece of a code so for now it was left as it is.
