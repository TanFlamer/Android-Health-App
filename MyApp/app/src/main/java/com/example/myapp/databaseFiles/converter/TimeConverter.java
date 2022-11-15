package com.example.myapp.databaseFiles.converter;

import androidx.room.TypeConverter;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;
import java.time.LocalTime;

public class TimeConverter {

    @TypeConverter
    public String fromTime(LocalTime localTime) {
        if (localTime == null) {
            return (null);
        }
        Gson gson = new Gson();
        Type type = new TypeToken<LocalTime>() {}.getType();
        return gson.toJson(localTime, type);
    }

    @TypeConverter
    public LocalTime toTime(String localTimeString) {
        if (localTimeString == null) {
            return (null);
        }
        Gson gson = new Gson();
        Type type = new TypeToken<LocalTime>() {}.getType();
        return gson.fromJson(localTimeString, type);
    }
}
