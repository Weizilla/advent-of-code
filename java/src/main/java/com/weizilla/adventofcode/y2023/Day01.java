package com.weizilla.adventofcode.y2023;

import com.google.common.base.Strings;
import com.weizilla.adventofcode.utils.InputReader;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class Day01 {
    private static final Map<String, Integer> WORDS = Map.of(
        "one", 1,
        "two", 2,
        "three", 3,
        "four", 4,
        "five", 5,
        "six", 6,
        "seven", 7,
        "eight", 8,
        "nine", 9
    );

    public static Object part1() {
        var input = InputReader.readInput(2023, 1);

        var result = input.stream()
            .map(Day01::toNums)
            .map(Day01::toSingleNum)
            .mapToInt(Integer::intValue)
            .sum();

        return result;
    }

    private static List<Integer> toNums(String input) {
        List<Integer> nums = new ArrayList<>();

        for (char c : input.toCharArray()) {
            if (Character.isDigit(c)) {
                String s = c + "";
                nums.add(Integer.parseInt(s));
            }
        }
        return nums;
    }

    public static String part2() {
        var input = InputReader.readInput(2023, 1);

        var result = input.stream()
            .map(Day01::toNums2)
            .map(Day01::toSingleNum)
            .mapToInt(Integer::intValue)
            .sum();

        return result + "";
    }

    private static List<Integer> toNums2(String input) {
        var result = new ArrayList<Integer>();
        var curr = input;

        for (int i = 0; i < curr.length(); i++) {
            char c = curr.charAt(i);
            if (Character.isDigit(c)) {
                result.add(Integer.parseInt(c + ""));
            } else {
                String substring = input.substring(i);
                for (Map.Entry<String, Integer> entry : WORDS.entrySet()) {
                    String w = entry.getKey();
                    if (substring.startsWith(w)) {
                        result.add(entry.getValue());
                    }
                }
            }
        }

        System.out.println("Input " + input + " nums " + result);

        return result;
    }

    private static Integer toSingleNum(List<Integer> ints) {
        int first = ints.getFirst();
        int last = ints.getLast();
        return first * 10 + last;
    }

    public static void main(String[] args) {
        Object result = Day01.part2();
        System.out.println(result);
    }
}
