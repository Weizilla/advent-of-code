package com.weizilla.adventofcode.utils;

import com.google.common.base.Strings;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class InputReader {
    public static List<String> readInput(int year, int day) {
        String inputPath = String.format("inputs/%d/day-%02d-input.txt", year, day);
        return readStrings(inputPath);
    }

    public static List<String> readInput(int year, int day, int example) {
        String inputPath = String.format("inputs/%d/day-%02d-example-%d.txt", year, day, example);
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


    public static void main(String[] args) {
        var reader = new InputReader();
        var strings = reader.readInput(2023, 1);
        System.out.println(strings);
    }
}
