package com.example.test;

import androidx.annotation.NonNull;
import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.PrimaryKey;
import androidx.room.TypeConverters;

import java.util.List;

@Entity(tableName = "user")
public class User {

    @PrimaryKey @NonNull
    private String username;

    @ColumnInfo(name = "password")
    private String password;

    @TypeConverters(ListConverter.class)
    @ColumnInfo(name = "musicPlaylist")
    private List<Test> testList;

    public User(@NonNull String username, String password, List<Test> testList) {
        this.username = username;
        this.password = password;
        this.testList = testList;
    }

    @NonNull
    public String getUsername() {
        return username;
    }

    public void setUsername(@NonNull String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public List<Test> getTestList() {
        return testList;
    }

    public void setTestList(List<Test> testList) {
        this.testList = testList;
    }
}
