package com.example.test;

import androidx.room.TypeConverter;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;
import java.util.List;

public class ListConverter {

    @TypeConverter
    public String fromTestList(List<Test> testList) {
        if (testList == null) {
            return (null);
        }
        Gson gson = new Gson();
        Type type = new TypeToken<List<Test>>() {}.getType();
        return gson.toJson(testList, type);
    }

    @TypeConverter
    public List<Test> toTestList(String testListString) {
        if (testListString == null) {
            return (null);
        }
        Gson gson = new Gson();
        Type type = new TypeToken<List<Test>>() {}.getType();
        return gson.fromJson(testListString, type);
    }
}
