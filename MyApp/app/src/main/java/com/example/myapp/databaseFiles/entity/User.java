package com.example.myapp.databaseFiles.entity;

import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.PrimaryKey;
import androidx.room.TypeConverters;

import com.example.myapp.databaseFiles.converter.IDConverter;

import java.util.List;

@Entity(tableName = "Users")
public class User {
    @PrimaryKey(autoGenerate = true)
    private int userID;

    @ColumnInfo(name = "username")
    private String username;

    @ColumnInfo(name = "password")
    private String password;

    @TypeConverters(IDConverter.class)
    @ColumnInfo(name = "playlist-id-list")
    private List<Integer> playlistIDList;

    @TypeConverters(IDConverter.class)
    @ColumnInfo(name = "sleep-id-list")
    private List<Integer> sleepIDList;

    @TypeConverters(IDConverter.class)
    @ColumnInfo(name = "sport-id-list")
    private List<Integer> sportIDList;

    public User(String username, String password, List<Integer> playlistIDList, List<Integer> sleepIDList, List<Integer> sportIDList) {
        this.username = username;
        this.password = password;
        this.playlistIDList = playlistIDList;
        this.sleepIDList = sleepIDList;
        this.sportIDList = sportIDList;
    }

    public int getUserID() {
        return userID;
    }

    public void setUserID(int userID) {
        this.userID = userID;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public List<Integer> getPlaylistIDList() {
        return playlistIDList;
    }

    public void setPlaylistIDList(List<Integer> playlistIDList) {
        this.playlistIDList = playlistIDList;
    }

    public List<Integer> getSleepIDList() {
        return sleepIDList;
    }

    public void setSleepIDList(List<Integer> sleepIDList) {
        this.sleepIDList = sleepIDList;
    }

    public List<Integer> getSportIDList() {
        return sportIDList;
    }

    public void setSportIDList(List<Integer> sportIDList) {
        this.sportIDList = sportIDList;
    }
}
