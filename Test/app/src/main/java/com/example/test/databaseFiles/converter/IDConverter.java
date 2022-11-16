package com.example.test.databaseFiles.converter;

import androidx.room.TypeConverter;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;
import java.util.List;

public class IDConverter {

    @TypeConverter
    public String fromID(List<Integer> idList) {
        if (idList == null) {
            return (null);
        }
        Gson gson = new Gson();
        Type type = new TypeToken<List<Integer>>() {}.getType();
        return gson.toJson(idList, type);
    }

    @TypeConverter
    public List<Integer> toID(String idListString) {
        if (idListString == null) {
            return (null);
        }
        Gson gson = new Gson();
        Type type = new TypeToken<List<Integer>>() {}.getType();
        return gson.fromJson(idListString, type);
    }
}
