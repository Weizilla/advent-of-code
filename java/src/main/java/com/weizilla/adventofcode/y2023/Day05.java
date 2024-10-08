package com.weizilla.adventofcode.y2023;

import com.weizilla.adventofcode.utils.Day;
import com.weizilla.adventofcode.utils.InputReader;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;

public class Day05 extends Day {
    @Override
    public Object part1() {
        var lines = InputReader.readStrings();

        var startSeeds = Arrays.stream(lines.get(0).split(":")[1].trim().split(" "))
            .map(Long::valueOf)
            .toList();

//        print("{}", startSeeds);

        var catMaps = parseMaps(lines);
//        print("{}", catMaps);

        Cat currCat = Cat.SEED;
        var currValues = new ArrayList<Long>(startSeeds);
        while (currCat != Cat.LOCATION) {
            var catMap = catMaps.get(currCat);

            var nextValues = new ArrayList<Long>();
            for (long currValue : currValues) {
                nextValues.add(catMap.getDestination(currValue));
            }
            currValues = nextValues;
            currCat = currCat.getDestination();
        }

        return currValues.stream().min(Long::compareTo).get();
    }

    private Map<Cat, CatMap> parseMaps(List<String> lines) {

        var maps = new HashMap<Cat, CatMap>();
        CatMap curr = null;
        for (int i = 1; i < lines.size(); i++) {
            String line = lines.get(i);
            if (line.isEmpty()) {
                continue;
            }

            if (line.contains("map")) {
                curr = new CatMap();
                var type = Cat.valueOf(line.split("-")[0].toUpperCase());
                maps.put(type, curr);
            } else {
                var splits = line.split(" ");
                curr.addRange(
                    Long.parseLong(splits[0]),
                    Long.parseLong(splits[1]),
                    Integer.parseInt(splits[2])
                );
            }
        }

        return maps;
    }
}

enum Cat {
    LOCATION(null),
    HUMIDITY(LOCATION),
    TEMPERATURE(HUMIDITY),
    LIGHT(TEMPERATURE),
    WATER(LIGHT),
    FERTILIZER(WATER),
    SOIL(FERTILIZER),
    SEED(SOIL);

    private final Cat destination;

    Cat(Cat destionation) {
        this.destination = destionation;
    }

    public Cat getDestination() {
        return destination;
    }
}

class CatMap {
    private final NavigableMap<Long, Integer> lengths;
    private final Map<Long, Long> destinations;

    public CatMap() {
        lengths = new TreeMap<>();
        destinations = new HashMap<>();
    }

    public void addRange(long destination, long source, int length) {
        lengths.put(source, length);
        destinations.put(source, destination);
    }

    public long getDestination(long source) {
        var entry = lengths.floorEntry(source);
        if (entry == null) {
            return source;
        }
        var start = entry.getKey();
        var length = entry.getValue();
        if (start + length < source) {
            return source;
        }
        return destinations.get(start) + source - start;
    }

    @Override
    public String toString() {
        return "CatMap{" +
            "lengths=" + lengths +
            ", destinations=" + destinations +
            '}';
    }
}
