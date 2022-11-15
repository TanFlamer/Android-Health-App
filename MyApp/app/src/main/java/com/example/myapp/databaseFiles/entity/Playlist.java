package com.example.myapp.databaseFiles.entity;

import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.PrimaryKey;
import androidx.room.TypeConverters;

import com.example.myapp.databaseFiles.converter.IDConverter;

import java.util.List;

@Entity(tableName = "Playlists")
public class Playlist {
    @PrimaryKey(autoGenerate = true)
    private int playlistID;

    @ColumnInfo(name = "playlist-name")
    private String playlistName;

    @TypeConverters(IDConverter.class)
    @ColumnInfo(name = "song-id-list")
    private List<Integer> songIDList;

    public Playlist(String playlistName, List<Integer> songIDList) {
        this.playlistName = playlistName;
        this.songIDList = songIDList;
    }

    public int getPlaylistID() {
        return playlistID;
    }

    public void setPlaylistID(int playlistID) {
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
}
