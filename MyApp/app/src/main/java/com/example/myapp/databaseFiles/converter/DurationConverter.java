package com.example.myapp.databaseFiles.converter;

import androidx.room.TypeConverter;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;
import java.time.Duration;

public class DurationConverter {

    @TypeConverter
    public String fromDuration(Duration duration) {
        if (duration == null) {
            return (null);
        }
        Gson gson = new Gson();
        Type type = new TypeToken<Duration>() {}.getType();
        return gson.toJson(duration, type);
    }

    @TypeConverter
    public Duration toDuration(String durationString) {
        if (durationString == null) {
            return (null);
        }
        Gson gson = new Gson();
        Type type = new TypeToken<Duration>() {}.getType();
        return gson.fromJson(durationString, type);
    }
}
