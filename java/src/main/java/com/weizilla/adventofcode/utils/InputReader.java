package com.weizilla.adventofcode.utils;

import com.google.common.base.Strings;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class InputReader {
    public static List<String> readStrings() {
        var runDay = Utils.getRunDay();
        String inputPath = String.format("inputs/%d/day-%02d-input.txt", runDay.year(), runDay.day());
        return readStrings(inputPath);
    }

    public static List<String> readStrings(int example) {
        var runDay = Utils.getRunDay();
        String inputPath = String.format("inputs/%d/day-%02d-example-%d.txt", runDay.year(), runDay.day(), example);
        return readStrings(inputPath);
    }


    private static List<String> readStrings(String inputPath) {
        try {
            Path path = Path.of("").toAbsolutePath().getParent().resolve(inputPath);
            return Files.readAllLines(path, StandardCharsets.UTF_8).stream()
                .filter(s -> !Strings.isNullOrEmpty(s))
                .toList();
        } catch (IOException e) {
            throw new RuntimeException("Error reading file", e);
        }
    }

    public static Matrix readMatrix() {
        try {
            var runDay = Utils.getRunDay();
            String inputPath = String.format("inputs/%d/day-%02d-input.txt", runDay.year(), runDay.day());
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

    public static Matrix readMatrix(int example) {
        try {
            var runDay = Utils.getRunDay();
            String inputPath = String.format("inputs/%d/day-%02d-example-%d.txt", runDay.year(), runDay.day(), example);
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
