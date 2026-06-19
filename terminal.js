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
  return line.trim().split(/\s+/).filter(Boolean);
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
`<div class="terminal-splash"><pre class="terminal-ascii">${escapeHTML(asciiLogo)}</pre><div class="terminal-info"><div class="terminal-user">rynn@world-machine</div><div class="terminal-rule">-------------------</div><div><span class="terminal-label">Name:</span> Rynn D</div><div><span class="terminal-label">Education:</span> Computer Science &amp; Linguistics B.A.</div><div><span class="terminal-label">Focus:</span> Games · Research · Art · Language Systems</div><div><span class="terminal-label">OS:</span> GitHub Pages</div><div><span class="terminal-label">WM:</span> World Machine</div><div><span class="terminal-label">Theme:</span> Black &amp; white retro web</div><div><span class="terminal-label">Shell:</span> custom browser terminal</div><div><span class="terminal-label">Editor:</span> VSCode / Neovim</div></div></div><div class="terminal-hint">Type <span>help</span> to see a list of commands.</div>`
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
`commands:
  help                 show this message
  kfetch               show system information
  ls [path]            list directory contents
  cd <path>            change directory
  cat <file>           print file contents
  pwd                  show current directory
  open <section>       scroll page to section
  clear                clear terminal

sections:
  about
  projects
  research
  art
  contact

examples:
  ls
  cd projects
  cat four-days.txt
  cd ..
  open art`
  );
}

function kfetch() {
  printHTML(
`<div class="terminal-splash"><pre class="terminal-ascii">${escapeHTML(asciiLogo)}</pre><div class="terminal-info"><div class="terminal-user">rynn@world-machine</div><div class="terminal-rule">-------------------</div><div><span class="terminal-label">Name:</span> Rynn D</div><div><span class="terminal-label">Education:</span> Computer Science &amp; Linguistics B.A.</div><div><span class="terminal-label">Focus:</span> Games · Research · Art · Language Systems</div><div><span class="terminal-label">OS:</span> GitHub Pages</div><div><span class="terminal-label">WM:</span> World Machine</div><div><span class="terminal-label">Theme:</span> Black &amp; white retro web</div><div><span class="terminal-label">Shell:</span> custom browser terminal</div><div><span class="terminal-label">Editor:</span> VSCode / Neovim</div></div></div>`
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

function cat(args) {
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

  print(node.content);
}

function pwd() {
  print(cwd);
}

function clear() {
  output.innerHTML = "";
}

function runCommand(line) {
  const tokens = tokenize(line);
  if (tokens.length === 0) return;

  const command = tokens[0];
  const args = tokens.slice(1);

  switch (command) {
    case "help":
      help();
      break;

    case "kfetch":
      kfetch();
      break;

    case "ls":
      ls(args);
      break;

    case "cd":
      cd(args);
      break;

    case "cat":
      cat(args);
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

form.addEventListener("submit", event => {
  event.preventDefault();

  const line = input.value;

  printHTML(`${promptHTML()} ${escapeHTML(line)}`);

  if (line.trim()) {
    history.push(line);
    historyIndex = history.length;
  }

  runCommand(line);
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