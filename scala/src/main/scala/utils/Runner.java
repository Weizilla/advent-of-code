package com.weizilla.adventofcode.utils;

import com.google.common.collect.Iterables;
import com.google.common.reflect.ClassPath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public class Runner {
    private static final Logger logger = LoggerFactory.getLogger(Runner.class);

    private static List<Class<? extends Day>> getAllClasses() throws Exception {
        return ClassPath.from(ClassLoader.getSystemClassLoader())
            .getAllClasses().stream()
            .filter(c -> c.getPackageName().contains("com.weizilla.adventofcode.y"))
            .filter(c -> !c.getName().contains("$"))
            .map(ClassPath.ClassInfo::load)
            .map(c -> (Class<? extends Day>) c)
            .sorted(Comparator.comparing(Class::getName))
            .collect(Collectors.toList());
    }

    private static List<Method> getAllMethods(Object object) {
        return Arrays.stream(object.getClass().getDeclaredMethods())
            .filter(m -> m.getName().equals("part1") || m.getName().equals("part2"))
            .sorted(Comparator.comparing(Method::getName))
            .toList();
    }

    public static void main(String[] args) {
        try {
            List<Class<? extends Day>> classes = getAllClasses();
            Class<? extends Day> newestClass = Iterables.getLast(classes);

            Integer example = args.length > 0 ? Integer.parseInt(args[0]) : null;

            logger.info("Class {} Example {}", newestClass, example);
            Day instance = (Day) newestClass.getConstructors()[0].newInstance(example);

            List<Method> methods = getAllMethods(instance);

            Method latestMethod = Iterables.getLast(methods);
            Object result = latestMethod.invoke(instance);

            String type = example != null ? ", example " + example : ", real";
            logger.info("Running year {}, day {}, {}{}, result: {}", instance.getYear(), instance.getDay(),
                latestMethod.getName(), type, result);

        } catch (Exception e) {
            logger.error("error", e);
        }
    }
}
