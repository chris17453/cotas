.PHONY: build run test clean restore cli web batch-parse

# Default: build everything
build:
	dotnet build

# Restore NuGet packages
restore:
	dotnet restore

# Run the web UI (from repo root so user_code/ and db/ paths work)
run: build
	dotnet run --project src/CoTAS.Web

# Run the web UI (development mode with hot reload)
dev: build
	dotnet watch --project src/CoTAS.Web

# Run all tests
test:
	dotnet test

# Run the CLI with a specific SRC file (usage: make cli SRC=user_code/HELLO.SRC)
cli: build
	dotnet run --project src/CoTAS.Cli -- $(SRC)

# Parse-only mode (usage: make parse SRC=user_code/HELLO.SRC)
parse: build
	dotnet run --project src/CoTAS.Cli -- $(SRC) --parse-only

# Batch parse all SRC files in user_code/
batch-parse: build
	dotnet run --project src/CoTAS.Cli -- --batch-parse user_code

# Clean build artifacts
clean:
	dotnet clean
