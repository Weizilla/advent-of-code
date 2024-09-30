package com.weizilla.adventofcode.y2023;

import com.weizilla.adventofcode.utils.Day;
import com.weizilla.adventofcode.utils.InputReader;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.weizilla.adventofcode.utils.InputReader.readStrings;

public class Day02 extends Day {
    record GameSet(int red, int green, int blue) {}
    record Game(int id, List<GameSet> gameSets) {
        public int maxRed() {
            return gameSets.stream()
                .mapToInt(GameSet::red)
                .max().orElse(0);
        }

        public int maxBlue() {
            return gameSets.stream()
                .mapToInt(GameSet::blue)
                .max().orElse(0);
        }

        public int maxGreen() {
            return gameSets.stream()
                .mapToInt(GameSet::green)
                .max().orElse(0);
        }
    }

    private static Pattern setRegex = Pattern.compile("((\\d+) red)|((\\d+) green)|((\\d+) blue)");

    @Override
    public Object part1() {
        int maxRed = 12;
        int maxGreen = 13;
        int maxBlue = 14;

        List<String> strings = InputReader.readStrings();
        int result = strings.stream()
            .map(Day02::parse)
            .filter(g -> g.maxBlue() <= maxBlue && g.maxGreen() <= maxGreen && g.maxRed() <= maxRed)
            .mapToInt(Game::id)
            .sum();

        return result;
    }

    @Override
    public Object part2() {
        List<String> strings = InputReader.readStrings();
        var result = strings.stream()
            .map(Day02::parse)
            .mapToInt(g -> g.maxRed() * g.maxBlue() * g.maxGreen())
            .sum();
        return result;
    }

    private static Game parse(String input) {
        String[] split = input.split(":");
        int id = Integer.parseInt(split[0].trim().split(" ")[1]);

        var gameSets = new ArrayList<GameSet>();
        for (String s : split[1].trim().split(";")) {
            Matcher matcher = setRegex.matcher(s);

            int red = 0;
            int green = 0;
            int blue = 0;

            while (matcher.find()) {
                red += numOrZero(matcher.group(2));
                green += numOrZero(matcher.group(4));
                blue += numOrZero(matcher.group(6));
            }

            var gameSet = new GameSet(red, green, blue);
            gameSets.add(gameSet);
        }

        Game game = new Game(id, gameSets);
        return game;
    }

    private static int numOrZero(String input) {
        return input != null ? Integer.parseInt(input) : 0;
    }


}
