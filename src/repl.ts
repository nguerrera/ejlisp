import { createEnvironment, evaluate } from "./evaluator";
import { createScanner, read } from "./reader";
import { question } from "readline-sync";

export function repl() {
  let done = false;
  const environment = createEnvironment();

  environment.exit = function exit() {
    done = true;
    return "Bye.";
  };

  while (!done) {
    try {
      let line = question("> ");

      const scanner = createScanner(line, () => {
        line = question("... ");
        scanner.appendInput("\n");
        scanner.appendInput(line.trimRight());
      });

      do {
        const datum = read(scanner);
        const value = evaluate(datum, environment);
        console.log(value); // we need a printer :)
      } while (!done && scanner.position != scanner.input.length);
    } catch (e) {
      console.log(e);
    }
  }
}
