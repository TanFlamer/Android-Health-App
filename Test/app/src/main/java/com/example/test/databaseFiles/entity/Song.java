package com.example.test.databaseFiles.entity;

import static androidx.room.ForeignKey.CASCADE;

import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.PrimaryKey;

@Entity(tableName = "Songs",
        foreignKeys = @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "user-id", onDelete = CASCADE))
public class Song {
    @PrimaryKey(autoGenerate = true)
    private Integer songID;

    @ColumnInfo(name = "song-name")
    private String songName;

    @ColumnInfo(name = "song-duration")
    private Integer songDuration;

    @ColumnInfo(name = "user-id", index = true)
    private Integer userID;


    public Song(String songName, Integer songDuration, Integer userID) {
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

    public Integer getSongDuration() {
        return songDuration;
    }

    public void setSongDuration(Integer songDuration) {
        this.songDuration = songDuration;
    }

    public Integer getUserID() {
        return userID;
    }

    public void setUserID(Integer userID) {
        this.userID = userID;
    }
}
