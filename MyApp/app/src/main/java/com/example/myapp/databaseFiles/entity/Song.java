package com.example.myapp.databaseFiles.entity;

import static androidx.room.ForeignKey.CASCADE;

import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Index;
import androidx.room.PrimaryKey;
import androidx.room.TypeConverters;

import com.example.myapp.databaseFiles.converter.DurationConverter;

import java.time.Duration;

@Entity(tableName = "Songs",
        indices = @Index("userID"),
        foreignKeys = @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "userID", onDelete = CASCADE))
public class Song {

    @PrimaryKey(autoGenerate = true)
    private Integer songID;

    private String songName;

    @TypeConverters(DurationConverter.class)
    private Duration songDuration;

    private Integer userID;

    public Song(String songName, Duration songDuration, Integer userID) {
        this.songName = songName;
        this.songDuration = songDuration;
        this.userID = userID;
    }

    public Integer getSongID() {
        return songID;
    }

    public void setSongID(Integer songID) {
        this.songID = songID;
    }

    public String getSongName() {
        return songName;
    }

    public void setSongName(String songName) {
        this.songName = songName;
    }

    public Duration getSongDuration() {
        return songDuration;
    }

    public void setSongDuration(Duration songDuration) {
        this.songDuration = songDuration;
    }

    public Integer getUserID() {
        return userID;
    }

    public void setUserID(Integer userID) {
        this.userID = userID;
    }
}
