package com.example.myapp.databaseFiles.dao;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

import com.example.myapp.databaseFiles.entity.Playlist;

import java.util.List;

@Dao
public interface PlaylistDao {

    @Insert
    long insert(Playlist playlist);

    @Update
    void update(Playlist playlist);

    @Delete
    void delete(Playlist playlist);

    @Query("SELECT * FROM Playlists WHERE playlistID=:playlistID")
    List<Playlist> getPlaylist(int playlistID);

    @Query("SELECT * FROM Playlists WHERE playlistName=:playlistName")
    List<Playlist> findPlaylist(String playlistName);

    @Query("SELECT * FROM Playlists WHERE userID=:userID")
    LiveData<List<Playlist>> getAllPlaylists(int userID);
}
