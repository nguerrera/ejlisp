import { compileFormToString, createEnvironment, evaluate } from "./evaluator";
import { EofError, createScanner, read } from "./reader";
import { intern, print } from "./primitives";
import { stdin, stdout } from "process";
import * as readline from "readline";

export async function readEvalPrintLoop() {
  let done = false;
  const rl = readline.createInterface(stdin, stdout);
  const environment = createEnvironment();

  environment.exit = function exit() {
    done = true;
    return intern("Bye.");
  };

  environment["to-string"] = function toString(x: any) {
    return x.toString();
  };

  environment["to-json"] = function toJson(x: any) {
    return JSON.stringify(x, undefined, 2);
  };

  environment.compile = function compile(form: unknown) {
    return compileFormToString(form);
  };

  while (!done) {
    let input = await question("> ");
    while (true) {
      try {
        readEvalPrint(input);
        break;
      } catch (e) {
        if (e instanceof EofError) {
          input += "\n";
          input += await question("... ");
          continue;
        }
        console.log(e);
        break;
      }
    }
  }

  rl.close();

  function readEvalPrint(input: string) {
    const scanner = createScanner(input);

    // Read all data from input before evaluating anything. If we hit EOF, we
    // don't want to cause side effects evaluating unfinished input.
    let data = [];
    while (scanner.position !== scanner.input.length) {
      const datum = read(scanner);
      data.push(datum);
    }

    for (const datum of data) {
      const value = evaluate(datum, environment);
      const printed = print(value);
      console.log(printed);
      if (done) {
        return;
      }
    }
  }

  function question(query: string): Promise<string> {
    return new Promise(r => rl.question(query, r));
  }
}
