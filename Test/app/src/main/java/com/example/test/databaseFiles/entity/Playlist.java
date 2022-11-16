package com.example.test.databaseFiles.entity;

import static androidx.room.ForeignKey.CASCADE;

import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.PrimaryKey;
import androidx.room.TypeConverters;

import com.example.test.databaseFiles.converter.IDConverter;

import java.util.List;

@Entity(tableName = "Playlists",
        foreignKeys = @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "user-id", onDelete = CASCADE))
public class Playlist {
    @PrimaryKey(autoGenerate = true)
    private Integer playlistID;

    @ColumnInfo(name = "playlist-name")
    private String playlistName;

    @TypeConverters(IDConverter.class)
    @ColumnInfo(name = "song-id-list")
    private List<Integer> songIDList;

    @ColumnInfo(name = "user-id", index = true)
    private Integer userID;

    public Playlist(String playlistName, List<Integer> songIDList, Integer userID) {
        this.playlistName = playlistName;
        this.songIDList = songIDList;
        this.userID = userID;
    }

    public Integer getPlaylistID() {
        return playlistID;
    }

    public void setPlaylistID(Integer playlistID) {
        this.playlistID = playlistID;
    }

    public String getPlaylistName() {
        return playlistName;
    }

    public void setPlaylistName(String playlistName) {
        this.playlistName = playlistName;
    }

    public List<Integer> getSongIDList() {
        return songIDList;
    }

    public void setSongIDList(List<Integer> songIDList) {
        this.songIDList = songIDList;
    }

    public Integer getUserID() {
        return userID;
    }

    public void setUserID(Integer userID) {
        this.userID = userID;
    }
}
