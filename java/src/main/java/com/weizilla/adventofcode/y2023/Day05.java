package com.weizilla.adventofcode.y2023;

import com.weizilla.adventofcode.utils.Day;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.NavigableMap;
import java.util.TreeMap;
import java.util.stream.Collectors;

public class Day05 extends Day {
    public Day05(Integer example) {
        super(example);
    }

    @Override
    public Object part1() {
        var lines = reader.readStrings();

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

    @Override
    public Object part2() {
        var lines = reader.readStrings();

        var startValues = Arrays.stream(lines.get(0).split(":")[1].trim().split(" "))
            .map(Long::valueOf)
            .toList();

        NavigableMap<Long, Long> currValues = new TreeMap<>();
        for (int i = 0; i < startValues.size(); i += 2) {
            currValues.put(startValues.get(i), startValues.get(i + 1));
        }

        print("{}", currValues);

        var catMaps = parseMaps(lines);
//        print("{}", catMaps);

        Cat currCat = Cat.SEED;
        while (currCat != Cat.LOCATION) {
            var catMap = catMaps.get(currCat);
            currValues = map(currValues, catMap);
            currCat = currCat.getDestination();
        }

        return currValues.navigableKeySet().first();

    }


    private NavigableMap<Long, Long> map(NavigableMap<Long, Long> value, CatMap mapping) {
        NavigableMap<Long, Long> result = new TreeMap<>();

        value.entrySet().stream()
            .map(e -> map(e, mapping))
            .forEach(result::putAll);

        return result;
    }

    private NavigableMap<Long, Long> map(Map.Entry<Long, Long> input, CatMap mapping) {
        long start = input.getKey();
        long len = input.getValue();
        long end = start + len;
        NavigableMap<Long, Long> result = new TreeMap<>();

        Mapping m = mapping.getMappings().floorEntry(start).getValue();

        // no overlap
        if (m == null || (m.sourceEnd() < start)) {
            result.put(start, len);
            return result;
        }

        // input is within one map
        if (start > m.source() && end < m.sourceEnd()) {
            long destStart = m.destination() + (start - m.source());
            result.put(destStart, len);
        }

        
        // handle range across multiple mapping

    }

    private enum Cat {
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

    private static class CatMap {
        private final NavigableMap<Long, Mapping> mappings;

        public CatMap() {
            mappings = new TreeMap<>();
        }

        public void addRange(long destination, long source, long length) {
            mappings.put(source, new Mapping(source, destination, length));
        }

        public long getDestination(long source) {
            var entry = mappings.floorEntry(source);
            if (entry == null) {
                return source;
            }
            var start = entry.getKey();
            var length = entry.getValue().length();
            var destination = entry.getValue().destination();
            if (start + length < source) {
                return source;
            }
            return destination + source - start;
        }

        public NavigableMap<Long, Mapping> getMappings() {
            return mappings;
        }


        @Override
        public String toString() {
            return "CatMap{" +
                "mappings=" + mappings +
                '}';
        }
    }

    private record Mapping(long source, long destination, long length) {
        public long sourceEnd() {
            return source + length;
        }

        public long destEnd() {
            return destination + length;
        }
    }

}
