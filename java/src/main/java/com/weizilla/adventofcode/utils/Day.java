package com.weizilla.adventofcode.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class Day {
    private static final Logger logger = LoggerFactory.getLogger(Day.class);
    protected final InputReader reader;
    private final int year;
    private final int day;
    private final Integer example;

    protected Day(Integer example) {
        RunDay runDay = Utils.getRunDay();
        this.year = runDay.year();
        this.day = runDay.day();
        this.example = example;
        reader = new InputReader(year, day, example);
    }

    public Object part1() {
        // not implemented yet!
        return null;
    }

    public Object part2() {
        return null;
    }

    public void printAlways(Object input) {
        printAlways("{}", input);
    }

    public void printAlways(String input, Object ... args) {
        logger.info(input, args);
    }

    public void print(Object input) {
        print("{}", input);
    }

    public void print(String input, Object ... args) {
        if (example != null) {
            printAlways(input, args);
        }
    }

    public int getYear() {
        return year;
    }

    public int getDay() {
        return day;
    }

    public Integer getExample() {
        return example;
    }
}
