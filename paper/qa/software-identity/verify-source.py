#!/usr/bin/env python3
from pathlib import Path
import json,hashlib,sys
root=Path(__file__).resolve().parents[2]
manifest=json.loads((root/'qa/software-identity/source-files.json').read_text())
source=root/'qa/software-source'
issues=[r['path'] for r in manifest if not (source/r['path']).is_file() or hashlib.sha256((source/r['path']).read_bytes()).hexdigest()!=r['sha256']]
actual={str(p.relative_to(source)) for p in source.rglob('*') if p.is_file()}
issues+=list(actual-{r['path'] for r in manifest})
print(json.dumps({'source_files':len(manifest),'issues':issues,'ok':not issues}))
sys.exit(bool(issues))
