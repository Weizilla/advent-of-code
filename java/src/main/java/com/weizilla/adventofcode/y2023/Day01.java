package com.weizilla.adventofcode.y2023;

import com.weizilla.adventofcode.utils.InputReader;

import java.util.ArrayList;
import java.util.List;

public class Day01 {
    public static String part1() {
        var input = InputReader.readInput(2023, 1, 1);

        var result = input.stream()
            .map(Day01::toNums)
            .map(Day01::toSingleNum)
            .mapToInt(Integer::intValue)
            .sum();

        return result + "";
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

    private static Integer toSingleNum(List<Integer> ints) {
        int first = ints.getFirst();
        int last = ints.getLast();
        return first * 10 + last;
    }


    public static void main(String[] args) {
        String result = Day01.part1();
        System.out.println(result);
    }
}
