package com.weizilla.adventofcode.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class Day {
    private static final Logger logger = LoggerFactory.getLogger(Day.class);
    private int year;
    private int day;

    protected Day() {
        RunDay runDay = Utils.getRunDay();
        this.year = runDay.year();
        this.day = runDay.day();
    }

    public Object part1() {
        // not implemented yet!
        return null;
    }

    public Object part2() {
        return null;
    }

    public void print(String input, Object ... args) {
        logger.info(input, args);
    }

    public int getYear() {
        return this.year;
    }

    public int getDay() {
        return this.day;
    }
}
