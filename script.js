const output = document.getElementById("terminal-output");
const form = document.getElementById("terminal-form");
const input = document.getElementById("terminal-input");

let cwd = "/";
let history = [];
let historyIndex = 0;

const asciiLogo = String.raw`
        __                    
       |  |                   
 _ __  |  |__   _   _  _ __  
| '__| | '_ \  | | | || '_ \ 
| |    | | | | | |_| || | | |
|_|    |_| |_|  \__,_||_| |_|
`;

const fileSystem = {
  type: "dir",
  children: {
    "about.txt": {
      type: "file",
      content:
`Rynn D
CS + Linguistics graduate.
Game developer, researcher, artist, and worldbuilder.

I build systems that turn structure into meaning:
language, games, memory, parsers, art, and interactive worlds.`
    },

    projects: {
      type: "dir",
      children: {
        "four-days.txt": {
          type: "file",
          content:
`Four Days

A meta-narrative game about memory, recursion, source code,
and reality editing.

Current design direction:
- story-first
- custom terminal/TUI source layer
- virtual filesystem
- patients/entities with source files, memories, dependencies,
  stack frames, heap objects, and garbage collection`
        },

        "third-tone-sandhi.txt": {
          type: "file",
          content:
`Mandarin Third Tone Sandhi Parser

An OCaml parser exploring how syntactic parsing, prosodic domains,
and constraint-based evaluation can model Mandarin third tone sandhi.

Themes:
- syntax-prosody mapping
- parsing ambiguity
- Optimality Theory-style constraint interaction
- executable linguistic analysis`
        }
      }
    },

    research: {
      type: "dir",
      children: {
        "interests.txt": {
          type: "file",
          content:
`Research Interests

- computational linguistics
- phonology and prosody
- syntax-prosody interfaces
- formal grammars and parsing
- digital humanities
- interactive systems for teaching theory`
        }
      }
    },

    art: {
      type: "dir",
      children: {
        "statement.txt": {
          type: "file",
          content:
`Art Statement

I want to make worlds where computation, memory, grief,
language, and beauty become visible systems.`
        }
      }
    },

    contact: {
      type: "dir",
      children: {
        "links.txt": {
          type: "file",
          content:
`Contact

Email: your-email@example.com
GitHub: https://github.com/yourusername
LinkedIn: https://linkedin.com/in/yourusername`
        }
      }
    }
  }
};

const commands = {
  help,
  kfetch,
  ls,
  cd,
  cat,
  pwd,
  clear,
  whoami,
  echo
};

function print(text = "") {
  output.innerHTML += text + "\n";
  scrollToBottom();
}

function printHTML(html = "") {
  output.innerHTML += html + "\n";
  scrollToBottom();
}

function escapeHTML(str) {
  return str.replace(/[&<>"']/g, char => ({
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    '"': "&quot;",
    "'": "&#039;"
  }[char]));
}

function scrollToBottom() {
  const terminal = document.querySelector(".terminal-window");
  terminal.scrollTop = terminal.scrollHeight;
}

function focusInput() {
  input.focus();
}

function getPrompt() {
  return `<span class="prompt">${escapeHTML(cwd)}&gt;</span>`;
}

function tokenize(commandLine) {
  return commandLine.trim().split(/\s+/).filter(Boolean);
}

function pathParts(path) {
  const fullPath = path.startsWith("/")
    ? path
    : cwd === "/"
      ? "/" + path
      : cwd + "/" + path;

  const parts = [];

  for (const part of fullPath.split("/")) {
    if (!part || part === ".") continue;
    if (part === "..") parts.pop();
    else parts.push(part);
  }

  return parts;
}

function resolvePath(path = ".") {
  let node = fileSystem;

  for (const part of pathParts(path)) {
    if (node.type !== "dir" || !node.children[part]) {
      return null;
    }

    node = node.children[part];
  }

  return node;
}

function normalizePath(path) {
  const parts = pathParts(path);
  return "/" + parts.join("/");
}

function help() {
  print(
`Available commands:
  kfetch        Shows system information
  ls [path]     Lists directory contents
  cd <path>     Changes directory
  cat <file>    Prints file contents
  pwd           Shows current directory
  whoami        Shows user
  echo <text>   Prints text
  clear         Clears terminal
  help          Displays this help message`
  );
}

function kfetch() {
  printHTML(
`<span class="green">rynn@fourdays.dev</span>
--------------------
<span class="yellow">Name:</span> Rynn D
<span class="yellow">Education:</span> Computer Science & Linguistics B.A.
<span class="yellow">Focus:</span> games, language systems, research, art
<span class="yellow">OS:</span> GitHub Pages
<span class="yellow">Shell:</span> custom browser terminal
<span class="yellow">Editor:</span> VSCode / Neovim
<span class="yellow">Projects:</span> Four Days, Mandarin 3TS Parser

${escapeHTML(asciiLogo)}`
  );
}

function ls(args) {
  const path = args[0] || ".";
  const node = resolvePath(path);

  if (!node) {
    print(`ls: cannot access '${path}': No such file or directory`);
    return;
  }

  if (node.type !== "dir") {
    print(path);
    return;
  }

  const names = Object.entries(node.children).map(([name, child]) => {
    return child.type === "dir"
      ? `<span class="blue">${escapeHTML(name)}/</span>`
      : escapeHTML(name);
  });

  printHTML(names.join("   "));
}

function cd(args) {
  const path = args[0] || "/";
  const node = resolvePath(path);

  if (!node) {
    print(`cd: no such file or directory: ${path}`);
    return;
  }

  if (node.type !== "dir") {
    print(`cd: not a directory: ${path}`);
    return;
  }

  cwd = normalizePath(path);
}

function cat(args) {
  const path = args[0];

  if (!path) {
    print("cat: missing file operand");
    return;
  }

  const node = resolvePath(path);

  if (!node) {
    print(`cat: ${path}: No such file or directory`);
    return;
  }

  if (node.type !== "file") {
    print(`cat: ${path}: Is a directory`);
    return;
  }

  print(escapeHTML(node.content));
}

function pwd() {
  print(cwd);
}

function clear() {
  output.innerHTML = "";
}

function whoami() {
  print("rynn");
}

function echo(args) {
  print(args.join(" "));
}

function runCommand(commandLine) {
  const tokens = tokenize(commandLine);
  if (tokens.length === 0) return;

  const command = tokens[0];
  const args = tokens.slice(1);

  if (!commands[command]) {
    print(`${command}: command not found`);
    return;
  }

  commands[command](args);
}

form.addEventListener("submit", event => {
  event.preventDefault();

  const commandLine = input.value;
  printHTML(`${getPrompt()} ${escapeHTML(commandLine)}`);

  if (commandLine.trim()) {
    history.push(commandLine);
    historyIndex = history.length;
  }

  runCommand(commandLine);
  input.value = "";
});

input.addEventListener("keydown", event => {
  if (event.key === "ArrowUp") {
    event.preventDefault();

    if (history.length > 0 && historyIndex > 0) {
      historyIndex--;
      input.value = history[historyIndex];
    }
  }

  if (event.key === "ArrowDown") {
    event.preventDefault();

    if (historyIndex < history.length - 1) {
      historyIndex++;
      input.value = history[historyIndex];
    } else {
      historyIndex = history.length;
      input.value = "";
    }
  }

  if (event.key === "Tab") {
    event.preventDefault();
  }
});

printHTML(`<span class="green">Type help to see a list of commands.</span>`);
printHTML(`${getPrompt()} help`);
help();