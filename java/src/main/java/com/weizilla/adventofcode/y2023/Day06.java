package com.weizilla.adventofcode.y2023;

import com.weizilla.adventofcode.utils.Day;

import java.util.ArrayList;
import java.util.List;

public class Day06 extends Day {
    public Day06(Integer example) {
        super(example);
    }

    @Override
    public Object part1() {
        List<String> lines = reader.readStrings();
        String[] timesLine = lines.get(0).split("\s+");
        String[] distanceLine = lines.get(1).split("\s+");

        List<Race> races = new ArrayList<>(timesLine.length - 1);
        for (int i = 1; i < timesLine.length; i++) {
            races.add(new Race(timesLine[i], distanceLine[i]));
        }

        print(races);

        // goal distance - (t * (time - t)) = 0
        // goal distance - t * time + t*t
        // t*t - time * t + goal dist = 0
        // a = 1, b = -time, c = goal dist
        // x = -b +/- sqrt(b * b - 4 * c) / 2

        int result = 1;

        for (Race r : races) {
            double time = r.time;
            double goalDist = r.distance + 1;
            double x1 = (time + Math.sqrt(time * time - (4 * goalDist))) / 2.0;
            double x2 = (time - Math.sqrt(time * time - (4 * goalDist))) / 2.0;
            print("{} {}", x1, x2);
            int a = (int) Math.ceil(Math.min(x1, x2));
            int b = (int) Math.floor(Math.max(x1, x2));

            result *= Math.abs(b - a) + 1;
        }

        return result;
    }

    @Override
    public Object part2() {
        List<String> lines = reader.readStrings();
        long raceTime = Long.parseLong(lines.get(0).split(":")[1].replaceAll(" ", ""));
        long raceDistance = Long.parseLong(lines.get(1).split(":")[1].replaceAll(" ", ""));

        List<Race> races = List.of(new Race(raceTime, raceDistance));

        print(races);

        // goal distance - (t * (time - t)) = 0
        // goal distance - t * time + t*t
        // t*t - time * t + goal dist = 0
        // a = 1, b = -time, c = goal dist
        // x = -b +/- sqrt(b * b - 4 * c) / 2

        int result = 1;

        for (Race r : races) {
            double time = r.time;
            double goalDist = r.distance + 1;
            double x1 = (time + Math.sqrt(time * time - (4 * goalDist))) / 2.0;
            double x2 = (time - Math.sqrt(time * time - (4 * goalDist))) / 2.0;
            print("{} {}", x1, x2);
            int a = (int) Math.ceil(Math.min(x1, x2));
            int b = (int) Math.floor(Math.max(x1, x2));

            result *= Math.abs(b - a) + 1;
        }

        return result;
    }

    private record Race(long time, long distance) {
        public Race(String time, String distance) {
            this(Long.parseLong(time), Long.parseLong(distance));
        }
    }

}
