package com.weizilla.adventofcode.utils;

import com.google.common.base.Strings;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class InputReader {
    public static List<String> readInput() {
        var runDay = Utils.getRunDay();
        String inputPath = String.format("inputs/%d/day-%02d-input.txt", runDay.year(), runDay.day());
        return readStrings(inputPath);
    }

    public static List<String> readInput(int example) {
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

}
