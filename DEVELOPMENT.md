# diversityGPT Development Workflow

## Quick Start

```r
# First time setup (run once)
source("dev/01_setup.R")

# Start each session
source("dev/02_load.R")
```

## Daily Development Workflow

### 1. Start Your Session
```r
# In RStudio, open diversityGPT.Rproj
source("dev/02_load.R")
```

### 2. Development Cycle
```r
# Make changes to R files
# Then reload and test:
devtools::load_all()
devtools::test()

# Or use shortcuts:
load_all()  # Reload package
test()      # Run all tests
check()     # Full package check
```

### 3. Before Committing
```r
# Run full check
devtools::check()

# Update documentation if needed
devtools::document()

# Run tests
devtools::test()
```

## Common Tasks

### Creating New Functions
```r
# Use the helper function
source("dev/04_document.R")
create_function("my_new_function")

# This creates:
# - R/my_new_function.R
# - tests/testthat/test-my_new_function.R
```

### Testing
```r
# Test everything
test()

# Test specific file
test_file("calculate_diversity")

# Interactive testing
source("dev/03_test.R")
test_interactive()
```

### Documentation
```r
# Update all docs
document()

# Build README
build_readme()

# Check documentation coverage
source("dev/04_document.R")
check_docs()
```

## API Key Management

### Setup (One Time)
1. Create `.Renviron` file in project root
2. Add your keys:
```
ANTHROPIC_API_KEY=your-anthropic-key
OPENAI_API_KEY=your-openai-key
```
3. Restart R session

### Check Keys
```r
check_api_keys()  # Run after loading dev/02_load.R
```

## Package Structure

```
diversityGPT/
├── R/                    # Function definitions
├── tests/testthat/       # Tests
├── man/                  # Documentation (auto-generated)
├── data/                 # Package data
├── data-raw/            # Scripts to create data
├── inst/
│   ├── prompts/         # LLM prompt templates
│   └── extdata/         # External data files
├── vignettes/           # Long-form documentation
└── dev/                 # Development scripts (not in package)
```

## Testing Philosophy

- **Test-Driven Development**: Write tests first when possible
- **Coverage Target**: Aim for >80% coverage
- **Test Types**:
  - Unit tests for each function
  - Integration tests for workflows
  - Mock tests for API calls

## Documentation Style

- **Roxygen2**: All functions must have roxygen documentation
- **Examples**: Include working examples (use \dontrun{} for API calls)
- **Vignettes**: Create vignettes for major workflows
- **README**: Keep updated with working examples

## Git Workflow

```bash
# Feature branch
git checkout -b feature/new-diversity-metric

# Make changes and test
# ... 

# Commit with descriptive message
git add .
git commit -m "Add Simpson diversity calculation with tests"

# Push and create PR
git push origin feature/new-diversity-metric
```

## Performance Considerations

- Profile slow functions with `profvis`
- Use vectorization over loops
- Cache expensive calculations
- Consider parallel processing for large datasets

## Debugging Tips

```r
# Debug a function
debug(calculate_diversity)

# Trace function calls
trace(consensus_diversity, browser)

# Check function environment
environment(calculate_diversity)

# Use browser() in code for breakpoints
```

## Release Checklist

- [ ] All tests pass
- [ ] R CMD check passes with no warnings
- [ ] Documentation is complete
- [ ] NEWS.md is updated
- [ ] Version number bumped in DESCRIPTION
- [ ] README examples work
- [ ] Vignettes build successfully