from pathlib import Path
import json

journal = Path("journal")
journal.mkdir(exist_ok=True)

files = []

for path in journal.iterdir():
    if not path.is_file():
        continue

    if path.name == "index.json":
        continue

    if path.suffix.lower() not in [".txt", ".md"]:
        continue

    files.append(path)

files = sorted(files, key=lambda p: p.name, reverse=True)

data = [
    {
        "name": path.name,
        "path": f"journal/{path.name}"
    }
    for path in files
]

(journal / "index.json").write_text(
    json.dumps(data, indent=2) + "\n",
    encoding="utf-8"
)

print(f"Wrote journal/index.json with {len(data)} entries.")