import { createEnvironment, evaluate } from "./evaluator";
import { createScanner, read } from "./reader";
import { intern, print } from "./primitives";
import { question } from "readline-sync";

export function repl() {
  let done = false;
  const environment = createEnvironment();
  environment.exit = function exit() {
    done = true;
    return intern("Bye.");
  };

  while (!done) {
    try {
      let line = readline("> ");

      const scanner = createScanner(line, () => {
        line = readline("... ");
        scanner.appendInput("\n");
        scanner.appendInput(line);
      });

      do {
        const datum = read(scanner);
        const value = evaluate(datum, environment);
        console.log(print(value));
      } while (!done && scanner.position != scanner.input.length);
    } catch (e) {
      console.log(e);
    }
  }
}

function readline(query: string) {
  return question(query, { keepWhitespace: true }).trimRight();
}
