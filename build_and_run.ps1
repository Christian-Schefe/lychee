# Build the compiler binary
cargo build --bin lychee-compiler

# Check if the build was successful
if ($LASTEXITCODE -ne 0) {
    Write-Host "Failed to build the compiler binary."
    exit $LASTEXITCODE
}

# Build the vm binary
cargo build --bin vm

# Check if the build was successful
if ($LASTEXITCODE -ne 0) {
    Write-Host "Failed to build the vm binary."
    exit $LASTEXITCODE
}

if ($args.Length -eq 0) {
    Write-Host "Please provide a path argument."
    exit 1
}

$path = $args[0]
$pathWithExtension = $args[1]

# Run the compiler binary
cargo run --bin lychee-compiler -- $path -o $pathWithExtension

# Check if the run was successful
if ($LASTEXITCODE -ne 0) {
    Write-Host "Failed to run the compiler binary."
    exit $LASTEXITCODE
}


# Run the vm binary
cargo run --bin vm -- $pathWithExtension

# Check if the run was successful
if ($LASTEXITCODE -ne 0) {
    Write-Host "Failed to run the vm binary."
    exit $LASTEXITCODE
}

Write-Host "Successfully built and ran both binaries."