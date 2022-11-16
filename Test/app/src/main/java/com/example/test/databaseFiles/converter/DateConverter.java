package com.example.test.databaseFiles.converter;

import androidx.room.TypeConverter;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;
import java.time.LocalDate;

public class DateConverter {

    @TypeConverter
    public String fromDate(LocalDate localDate) {
        if (localDate == null) {
            return (null);
        }
        Gson gson = new Gson();
        Type type = new TypeToken<LocalDate>() {}.getType();
        return gson.toJson(localDate, type);
    }

    @TypeConverter
    public LocalDate toDate(String localDateString) {
        if (localDateString == null) {
            return (null);
        }
        Gson gson = new Gson();
        Type type = new TypeToken<LocalDate>() {}.getType();
        return gson.fromJson(localDateString, type);
    }
}
