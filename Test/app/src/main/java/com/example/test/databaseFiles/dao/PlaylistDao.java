package com.example.test.databaseFiles.dao;

import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

import com.example.test.databaseFiles.entity.Playlist;

import java.util.List;

@Dao
public interface PlaylistDao {

    @Insert
    void insert(Playlist playlist);

    @Update
    void update(Playlist playlist);

    @Delete
    void delete(Playlist playlist);

    @Query("SELECT * FROM Playlists WHERE playlistID=:playlistID")
    List<Playlist> findPlaylist(int playlistID);

    @Query("SELECT * FROM Playlists")
    List<Playlist> getAllPlaylists();
}
