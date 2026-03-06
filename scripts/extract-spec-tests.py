#!/usr/bin/env python3
"""
Extract consensus-spec-tests SSZ test data into JSON format for Common Lisp testing.

This script reads the snappy-compressed test data and YAML files,
then outputs JSON that can be easily parsed by Common Lisp.
"""

import json
import os
import sys
from pathlib import Path

import snappy
import yaml


def decompress_ssz(path: Path) -> list[int]:
    """Decompress a .ssz_snappy file and return bytes as list of ints."""
    with open(path, 'rb') as f:
        compressed = f.read()
        decompressed = snappy.decompress(compressed)
        return list(decompressed)


def parse_yaml_value(path: Path) -> any:
    """Parse a value.yaml file."""
    with open(path) as f:
        content = f.read().strip()
        # Handle the ... at the end of YAML files
        if content.endswith('...'):
            content = content[:-3].strip()
        return yaml.safe_load(content)


def parse_meta(path: Path) -> dict:
    """Parse a meta.yaml file."""
    with open(path) as f:
        return yaml.safe_load(f)


def hex_to_bytes(hex_str: str) -> list[int]:
    """Convert hex string (0x...) to list of bytes."""
    if hex_str.startswith('0x'):
        hex_str = hex_str[2:]
    return [int(hex_str[i:i+2], 16) for i in range(0, len(hex_str), 2)]


def extract_test_case(test_dir: Path, is_valid: bool) -> dict:
    """Extract a single test case."""
    result = {
        'name': test_dir.name,
        'valid': is_valid,
    }

    # Get serialized data
    ssz_file = test_dir / 'serialized.ssz_snappy'
    if ssz_file.exists():
        result['serialized'] = decompress_ssz(ssz_file)

    if is_valid:
        # Get expected value
        value_file = test_dir / 'value.yaml'
        if value_file.exists():
            result['value'] = parse_yaml_value(value_file)

        # Get expected root
        meta_file = test_dir / 'meta.yaml'
        if meta_file.exists():
            meta = parse_meta(meta_file)
            if 'root' in meta:
                result['root'] = hex_to_bytes(meta['root'])

    return result


def extract_type_tests(type_dir: Path) -> dict:
    """Extract all tests for a type (e.g., uints, boolean)."""
    result = {
        'type': type_dir.name,
        'valid': [],
        'invalid': [],
    }

    valid_dir = type_dir / 'valid'
    if valid_dir.exists():
        for test_case in sorted(valid_dir.iterdir()):
            if test_case.is_dir():
                result['valid'].append(extract_test_case(test_case, True))

    invalid_dir = type_dir / 'invalid'
    if invalid_dir.exists():
        for test_case in sorted(invalid_dir.iterdir()):
            if test_case.is_dir():
                result['invalid'].append(extract_test_case(test_case, False))

    return result


def main():
    # Find the consensus-spec-tests directory
    script_dir = Path(__file__).parent
    project_dir = script_dir.parent

    # Try refs/ directory first, then project root
    spec_tests = project_dir / 'refs' / 'consensus-spec-tests'
    if not spec_tests.exists():
        spec_tests = project_dir / 'consensus-spec-tests'
    if not spec_tests.exists():
        print(f"Error: consensus-spec-tests not found at {spec_tests}", file=sys.stderr)
        sys.exit(1)

    ssz_generic = spec_tests / 'tests' / 'general' / 'phase0' / 'ssz_generic'
    if not ssz_generic.exists():
        print(f"Error: ssz_generic tests not found at {ssz_generic}", file=sys.stderr)
        sys.exit(1)

    # Extract all test types
    all_tests = {}

    for type_dir in sorted(ssz_generic.iterdir()):
        if type_dir.is_dir():
            print(f"Extracting {type_dir.name}...", file=sys.stderr)
            all_tests[type_dir.name] = extract_type_tests(type_dir)

    # Output JSON
    output_file = project_dir / 'test' / 'spec-test-data.json'
    output_file.parent.mkdir(exist_ok=True)

    with open(output_file, 'w') as f:
        json.dump(all_tests, f, indent=2)

    print(f"Extracted tests to {output_file}", file=sys.stderr)

    # Print summary
    for type_name, type_tests in all_tests.items():
        valid_count = len(type_tests['valid'])
        invalid_count = len(type_tests['invalid'])
        print(f"  {type_name}: {valid_count} valid, {invalid_count} invalid", file=sys.stderr)


if __name__ == '__main__':
    main()
