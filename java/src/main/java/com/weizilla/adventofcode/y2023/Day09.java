package com.weizilla.adventofcode.y2023;

import com.weizilla.adventofcode.utils.Day;

import java.util.Arrays;
import java.util.List;

public class Day09 extends Day {
    public Day09(Integer example) {
        super(example);
    }

    @Override
    public Object part1() {
        List<String> strings = reader.readStrings();
        List<List<Long>> input = strings.stream()
            .map(s -> Arrays.stream(s.split("\s+"))
                .map(Long::parseLong)
                .toList())
            .toList();

        List<Long> next = input.stream()
            .map(this::findNext)
            .toList();

        return next.stream().mapToLong(Long::longValue).sum();
    }

    private long findNext(List<Long> input) {
        return 0;
    }
}
