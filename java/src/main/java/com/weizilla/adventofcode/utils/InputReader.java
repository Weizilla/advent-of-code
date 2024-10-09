package com.weizilla.adventofcode.utils;

import com.google.common.base.Strings;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.Executors;

public class InputReader {
    private final int year;
    private final int day;
    private final Integer example;

    public InputReader(int year, int day, Integer example) {
        this.year = year;
        this.day = day;
        this.example = example;
    }

    private String getInputPath() {
        String inputPath = example != null
            ? String.format("inputs/%d/day-%02d-example-%d.txt", year, day, example)
            : String.format("inputs/%d/day-%02d-input.txt", year, day);
        return inputPath;
    }

    public List<String> readStrings() {
        try {
            String inputPath = getInputPath();
            Path path = Path.of("").toAbsolutePath().getParent().resolve(inputPath);
            return Files.readAllLines(path, StandardCharsets.UTF_8).stream()
                .filter(s -> !Strings.isNullOrEmpty(s))
                .toList();
        } catch (IOException e) {
            throw new RuntimeException("Error reading file", e);
        }
    }

    public Matrix readMatrix() {
        try {
            String inputPath = getInputPath();
            Path path = Path.of("").toAbsolutePath().getParent().resolve(inputPath);
            var stream = Files.readAllLines(path, StandardCharsets.UTF_8).stream()
                .filter(s -> !Strings.isNullOrEmpty(s))
                .toList();

            var matrix = new Matrix();
            for (int y = 0; y < stream.size(); y++) {
                String line = stream.get(y);
                for (int x = 0; x < line.length(); x++) {
                    String value = line.charAt(x) + "";
                    matrix.put(x, y, value);
                }
            }
            return matrix;
        } catch (IOException e) {
            throw new RuntimeException("Error reading file", e);
        }
    }
}
