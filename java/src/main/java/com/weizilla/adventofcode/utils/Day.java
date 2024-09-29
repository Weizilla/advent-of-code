package com.weizilla.adventofcode.utils;

public abstract class Day {
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

    public int getYear() {
        return this.year;
    }

    public int getDay() {
        return this.day;
    }
}
