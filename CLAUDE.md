# M3 Project Rules

## Commit messages

Every commit message MUST be prefixed with a GitHub issue number. Format:

```
#<issue>: <description>
```

Examples:
- `#26: npm: bundle schemas in package`
- `#29: README: drop "Every language" tagline`

If a commit addresses multiple issues, use the primary issue number as prefix.

A git hook enforces this. After cloning, install it:

```bash
cp bin/hooks/commit-msg .git/hooks/commit-msg
```
