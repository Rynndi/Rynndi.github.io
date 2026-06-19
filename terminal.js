const output = document.getElementById("terminal-output");
const form = document.getElementById("terminal-form");
const input = document.getElementById("terminal-input");
const terminalScreen = document.querySelector(".terminal-screen");

if (!output || !form || !input || !terminalScreen) {
  console.error("Terminal elements not found. Check your HTML IDs.");
}

let cwd = "/";
let history = [];
let historyIndex = 0;

const asciiLogo = String.raw`
██████╗ ██╗   ██╗███╗   ██╗███╗   ██╗
██╔══██╗╚██╗ ██╔╝████╗  ██║████╗  ██║
██████╔╝ ╚████╔╝ ██╔██╗ ██║██╔██╗ ██║
██╔══██╗  ╚██╔╝  ██║╚██╗██║██║╚██╗██║
██║  ██║   ██║   ██║ ╚████║██║ ╚████║
╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═══╝
`;

const fileSystem = {
  type: "dir",
  children: {
    lexicon: {
  type: "dir",
  children: {
    "determiners.txt": { type: "file", staticPath: "lexicon/determiners.txt" },
    "wh_determiners.txt": { type: "file", staticPath: "lexicon/wh_determiners.txt" },
    "pronouns.txt": { type: "file", staticPath: "lexicon/pronouns.txt" },
    "nouns.txt": { type: "file", staticPath: "lexicon/nouns.txt" },
    "adjectives.txt": { type: "file", staticPath: "lexicon/adjectives.txt" },
    "adverbs.txt": { type: "file", staticPath: "lexicon/adverbs.txt" },
    "verbs.txt": { type: "file", staticPath: "lexicon/verbs.txt" },
    "participles.txt": { type: "file", staticPath: "lexicon/participles.txt" },
    "gerunds.txt": { type: "file", staticPath: "lexicon/gerunds.txt" },
    "auxiliaries.txt": { type: "file", staticPath: "lexicon/auxiliaries.txt" },
    "modals.txt": { type: "file", staticPath: "lexicon/modals.txt" },
    "tense_words.txt": { type: "file", staticPath: "lexicon/tense_words.txt" },
    "prepositions.txt": { type: "file", staticPath: "lexicon/prepositions.txt" },
    "complementizers.txt": { type: "file", staticPath: "lexicon/complementizers.txt" },
    "question_auxiliaries.txt": { type: "file", staticPath: "lexicon/question_auxiliaries.txt" },
    "wh_question_auxiliaries.txt": { type: "file", staticPath: "lexicon/wh_question_auxiliaries.txt" },
    "wh_prepositions.txt": { type: "file", staticPath: "lexicon/wh_prepositions.txt" },
    "coordinators.txt": { type: "file", staticPath: "lexicon/coordinators.txt" },
    "trace_words.txt": { type: "file", staticPath: "lexicon/trace_words.txt" },
    "numbers.txt": { type: "file", staticPath: "lexicon/numbers.txt" },
    "units.txt": { type: "file", staticPath: "lexicon/units.txt" },
    "currencies.txt": { type: "file", staticPath: "lexicon/currencies.txt" }
  }
},

    source: {
      type: "dir",
      children: {
        "parser.ml": {
          type: "file",
          staticPath: "files/parser.ml"
        }
      }
    },

    "about.txt": {
      type: "file",
      content:
`Rynn D
CS + Linguistics · Games · Research · Art

I build systems that turn structure into meaning:
language, games, memory, parsers, art, and interactive worlds.

This site is a small fake operating system for exploring my work.`
    },

    projects: {
      type: "dir",
      children: {
        "four-days.txt": {
          type: "file",
          content:
`Four Days

A meta-narrative game about memory, recursion, source files,
and reality editing through a custom terminal-like interface.

Design direction:
- story-first
- virtual filesystem
- patients/entities as typed objects
- source files, memories, dependencies, permissions
- stack, heap, pointers, garbage collection
- every puzzle should reveal character and worldbuilding`
        },

        "third-tone-sandhi.txt": {
          type: "file",
          content:
`Mandarin Third Tone Sandhi Parser

An OCaml parser exploring Mandarin third tone sandhi through
syntax-prosody mapping, parsing, and constraint-based alternations.

Core idea:
Different parses can map onto different prosodic domains,
which can affect where third tone sandhi applies.`
        },

        "unity-tools.txt": {
          type: "file",
          content:
`Unity / Interactive Systems

Interests:
- game tools
- narrative systems
- custom UI
- world state machines
- source-layer interfaces
- player-facing debugging metaphors`
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
- formal grammars
- parsing
- syntax-prosody interfaces
- digital humanities
- tools for making theory executable`
        },

        "statement.txt": {
          type: "file",
          content:
`Research Statement Fragment

I am interested in systems that make hidden linguistic structure visible:
parsers, grammars, prosodic domains, constraints, and interfaces that
allow people to inspect how meaning is built.`
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
language, and beauty become visible systems.

My art interests include:
- character design
- 3D modeling
- worldbuilding
- game environments
- emotional systems`
        },

        "four-days-world.txt": {
          type: "file",
          content:
`Four Days Worldbuilding

Themes:
- memory as storage
- grief as recursion
- identity as a pointer
- forgotten things as garbage collection
- gods as system processes
- reality as a compiled artifact`
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
GitHub: github.com/yourusername
LinkedIn: linkedin.com/in/yourusername`
        }
      }
    }
  }
};

function focusTerminal() {
  input.focus();
}

function escapeHTML(str) {
  return String(str).replace(/[&<>"']/g, char => ({
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    '"': "&quot;",
    "'": "&#039;"
  }[char]));
}

function scrollTerminal() {
  terminalScreen.scrollTop = terminalScreen.scrollHeight;
}

function print(text = "") {
  output.innerHTML += escapeHTML(text) + "\n";
  scrollTerminal();
}

function printHTML(html = "") {
  output.innerHTML += html + "\n";
  scrollTerminal();
}

function promptHTML() {
  return `<span class="terminal-prompt">${escapeHTML(cwd)}&gt;</span>`;
}

function tokenize(line) {
  const tokens = [];
  const regex = /"([^"]*)"|'([^']*)'|(\S+)/g;

  let match;

  while ((match = regex.exec(line)) !== null) {
    tokens.push(match[1] || match[2] || match[3]);
  }

  return tokens;
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

    if (part === "..") {
      parts.pop();
    } else {
      parts.push(part);
    }
  }

  return parts;
}

function normalizePath(path) {
  const parts = pathParts(path);
  return "/" + parts.join("/");
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

function renderSplash() {
  output.innerHTML = "";

  printHTML(
`<div class="terminal-splash"><pre class="terminal-ascii">${escapeHTML(asciiLogo)}</pre><div class="terminal-info"><div class="terminal-user">rynn@rynn-os</div><div class="terminal-rule">-------------------</div><div><span class="terminal-label">Name:</span> Rynn D</div><div><span class="terminal-label">Education:</span> Computer Science &amp; Linguistics B.A.</div><div><span class="terminal-label">Focus:</span> Games · Research · Art · Language Systems</div><div><span class="terminal-label">OS:</span> GitHub Pages</div><div><span class="terminal-label">WM:</span> World Machine</div><div><span class="terminal-label">Theme:</span> Black &amp; white retro web</div><div><span class="terminal-label">Shell:</span> custom browser terminal</div><div><span class="terminal-label">Editor:</span> VSCode / Neovim</div></div></div><div class="terminal-hint">Type <span>help</span> to see a list of commands.</div>`
  );
}

function scrollToSection(id) {
  const section = document.getElementById(id);

  if (!section) {
    print(`open: section not found: ${id}`);
    return;
  }

  section.scrollIntoView({ behavior: "smooth", block: "start" });
  print(`opening #${id}`);
}

function help() {
  print(
`Available commands:
rfetch  Shows system information
ls      List directory contents
cat     Prints contents of files
cd      Changes directory
parse   Generates syntax trees for a sentence
help    Display this help message`
  );
}

function rfetch() {
  printHTML(
`<div class="terminal-splash"><pre class="terminal-ascii">${escapeHTML(asciiLogo)}</pre><div class="terminal-info"><div class="terminal-user">rynn@rynn-os</div><div class="terminal-rule">-------------------</div><div><span class="terminal-label">Name:</span> Rynn D</div><div><span class="terminal-label">Education:</span> Computer Science &amp; Linguistics B.A.</div><div><span class="terminal-label">Focus:</span> Games · Research · Art · Language Systems</div><div><span class="terminal-label">OS:</span> GitHub Pages</div><div><span class="terminal-label">WM:</span> World Machine</div><div><span class="terminal-label">Theme:</span> Black &amp; white retro web</div><div><span class="terminal-label">Shell:</span> custom browser terminal</div><div><span class="terminal-label">Editor:</span> VSCode / Neovim</div></div></div>`
  );
}

function ls(args) {
  const path = args[0] || ".";
  const node = resolvePath(path);

  if (!node) {
    print(`ls: cannot access '${path}': no such file or directory`);
    return;
  }

  if (node.type === "file") {
    print(path);
    return;
  }

  const names = Object.entries(node.children).map(([name, child]) => {
    if (child.type === "dir") {
      return `<span class="terminal-dir">${escapeHTML(name)}/</span>`;
    }

    return `<span class="terminal-file">${escapeHTML(name)}</span>`;
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

async function cat(args) {
  const path = args[0];

  if (!path) {
    print("cat: missing file operand");
    return;
  }

  const node = resolvePath(path);

  if (!node) {
    print(`cat: ${path}: no such file or directory`);
    return;
  }

  if (node.type !== "file") {
    print(`cat: ${path}: is a directory`);
    return;
  }

  if (node.content) {
    print(node.content);
    return;
  }

  if (node.staticPath) {
    try {
      const response = await fetch(node.staticPath);

      if (!response.ok) {
        print(`cat: could not read ${path}`);
        return;
      }

      const text = await response.text();
      print(text);
    } catch (error) {
      print(`cat: error reading ${path}`);
    }

    return;
  }

  print(`cat: ${path}: empty file`);
}

function pwd() {
  print(cwd);
}

function clear() {
  output.innerHTML = "";
}

function parseSentence(args) {
  const sentence = args.join(" ").trim();

  if (!sentence) {
    print('usage: parse "The man saw the boy with the telescope"');
    return;
  }

  if (typeof window.parseEnglish === "function") {
    try {
      const result = window.parseEnglish(sentence);
      print(result);
    } catch (error) {
      print(`parse: parser error: ${error.message}`);
    }

    return;
  }

  const normalized = sentence.toLowerCase();

  if (normalized === "the man saw the boy with the telescope") {
    print(
`Parsing: ${sentence}

2 parses found.

Parse 1:
[S [NP the man] [VP [V saw] [NP [NP the boy] [PP with the telescope]]]]

Parse 2:
[S [NP the man] [VP [VP [V saw] [NP the boy]] [PP with the telescope]]]`
    );
    return;
  }

  print(
`Parsing: ${sentence}

Parser module not loaded yet.

Right now, this website can read the OCaml source with:

  cat source/parser.ml

But the parse command cannot execute the OCaml parser until the parser is compiled
into browser JavaScript and exposes:

  window.parseEnglish(sentence)`
  );
}

async function runCommand(line) {
  const tokens = tokenize(line);
  if (tokens.length === 0) return;

  const command = tokens[0].toLowerCase();
  const args = tokens.slice(1);

  switch (command) {
    case "parse":
      parseSentence(args);
      break;

    case "help":
      help();
      break;

    case "rfetch":
      rfetch();
      break;

    case "ls":
      ls(args);
      break;

    case "cd":
      cd(args);
      break;

    case "cat":
      await cat(args);
      break;

    case "pwd":
      pwd();
      break;

    case "clear":
      clear();
      break;

    case "open":
      if (!args[0]) {
        print("usage: open <section>");
      } else {
        scrollToSection(args[0]);
      }
      break;

    case "about":
    case "projects":
    case "research":
    case "art":
    case "contact":
      scrollToSection(command);
      break;

    default:
      print(`${command}: command not found`);
      break;
  }
}

form.addEventListener("submit", async event => {
  event.preventDefault();

  const line = input.value;

  printHTML(`${promptHTML()} ${escapeHTML(line)}`);

  if (line.trim()) {
    history.push(line);
    historyIndex = history.length;
  }

  await runCommand(line);
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

terminalScreen.addEventListener("click", focusTerminal);

renderSplash();
focusTerminal();