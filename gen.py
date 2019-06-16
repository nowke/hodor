import argparse
import nbformat
from nbconvert import MarkdownExporter
from pathlib import Path

scm_template = """
# {}

```scheme
{}
```
"""

def generate_sicp():
    sicp_path = Path('docs/SICP')
    scm_files = sorted(sicp_path.glob('**/*.scm'))
    rkt_files = sorted(sicp_path.glob('**/*.rkt'))
    for scm_file in scm_files + rkt_files:
        content = scm_file.read_text()
        title = scm_file.stem
        markdown = scm_template.format(title, content)
        md_file = scm_file.parent / f'{scm_file.stem}.md'
        md_file.write_text(markdown)

def generate_clrs():
    clrs_path = Path('docs/CLRS')
    nb_files = sorted(clrs_path.glob('**/*.ipynb'))
    for nb_file in nb_files:
        md_exporter = MarkdownExporter()
        with open(nb_file) as f:
            markdown, _ = md_exporter.from_file(f)
        
        title = nb_file.stem
        md_file = nb_file.parent / f'{title}.md'
        print(md_file)
        md_file.write_text(markdown)


def generate():
    generators = {
        'SICP': generate_sicp,
        'CLRS': generate_clrs
    }

    parser = argparse.ArgumentParser()
    parser.add_argument("--item")
    args = parser.parse_args()
    if args.item == 'all':
        for key in generators:
            generators[key]()
    elif args.item in generators:
        generators[args.item]()
    else:
        raise ValueError(f'Key `{args.item}`` not found')


if __name__ == "__main__":
    generate()
