package com.weizilla.adventofcode.y2023;

import com.google.common.collect.Range;
import com.weizilla.adventofcode.utils.Day;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;

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

        List<Range<Long>> currValues = new ArrayList<>();
        for (int i = 0; i < startValues.size(); i += 2) {
            currValues.add(Range.closed(startValues.get(i), startValues.get(i) + startValues.get(i + 1)));
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

        return currValues.stream().mapToLong(Range::lowerEndpoint).min();
    }


    private List<Range<Long>> map(List<Range<Long>> values, CatMap mapping) {
        List<Range<Long>> result = new ArrayList<>();

        values.stream()
            .map(e -> map(e, mapping))
            .forEach(result::addAll);

        return result;
    }

    private List<Range<Long>> map(Range<Long> ranges, CatMap allMappings) {
        List<Range<Long>> unmapped = List.of(ranges);
        List<Range<Long>> mapped = new ArrayList<>();

        for (Mapping mapping : allMappings.getMappings().values()) {

            List<Range<Long>> newUnmapped = new ArrayList<>();
            for (Range<Long> curr : unmapped) {
                if (curr.upperEndpoint() < mapping.source() || curr.lowerEndpoint() > mapping.sourceEnd()) {
                    newUnmapped.add(curr);
                    continue;
                }

                long centerLowerMapped = 0;
                long centerUpperMapped = 0;

                if (curr.lowerEndpoint() < mapping.source()) {
                    // ----
                    //  curr  ----
                    //        mapping
                    Range<Long> lowerDiff = Range.closed(curr.lowerEndpoint(), mapping.source());
                    newUnmapped.add(lowerDiff);
                    centerLowerMapped = mapping.destination();
                } else if (curr.lowerEndpoint() >= mapping.source()) {
                    //        ----
                    // ----   mapping
                    //  curr
                    centerLowerMapped = curr.lowerEndpoint() - mapping.source() + mapping.destination();
                }

                if (curr.upperEndpoint() <= mapping.sourceEnd()) {
                    // curr
                    // ----  mapping
                    //       ----

                    centerUpperMapped = mapping.destEnd() - (mapping.sourceEnd() - curr.upperEndpoint());
                } else if (curr.upperEndpoint() > mapping.sourceEnd()) {
                    //        mapping
                    // curr   ----
                    // ----
                    centerUpperMapped = mapping.destEnd();
                    Range<Long> upperDiff = Range.closed(mapping.sourceEnd(), curr.upperEndpoint());
                    newUnmapped.add(upperDiff);
                }

                mapped.add(Range.closed(centerLowerMapped, centerUpperMapped));
            }
            unmapped = newUnmapped;
        }

        mapped.addAll(unmapped);


        return mapped;
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
