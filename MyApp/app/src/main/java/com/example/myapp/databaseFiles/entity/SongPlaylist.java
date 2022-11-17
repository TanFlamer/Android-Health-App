package com.example.myapp.databaseFiles.entity;

import static androidx.room.ForeignKey.CASCADE;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Index;

@Entity(tableName = "SongPlaylist",
        primaryKeys = { "playlistID", "songID" },
        indices = { @Index("playlistID"), @Index("songID") },
        foreignKeys = { @ForeignKey(entity = Playlist.class, parentColumns = "playlistID", childColumns = "playlistID", onDelete = CASCADE),
                        @ForeignKey(entity = Song.class, parentColumns = "songID", childColumns = "songID", onDelete = CASCADE) })
public class SongPlaylist {

    @NonNull
    private Integer playlistID;

    @NonNull
    private Integer songID;

    public SongPlaylist(@NonNull Integer playlistID, @NonNull Integer songID){
        this.playlistID = playlistID;
        this.songID = songID;
    }

    @NonNull
    public Integer getPlaylistID() {
        return playlistID;
    }

    public void setPlaylistID(@NonNull Integer playlistID) {
        this.playlistID = playlistID;
    }

    @NonNull
    public Integer getSongID() {
        return songID;
    }

    public void setSongID(@NonNull Integer songID) {
        this.songID = songID;
    }
}
