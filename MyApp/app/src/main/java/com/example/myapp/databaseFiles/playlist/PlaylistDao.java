package com.example.myapp.databasefiles.playlist;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

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
    Playlist getPlaylist(int playlistID);

    @Query("SELECT * FROM Playlists WHERE userID=:userID AND playlistName=:playlistName")
    Playlist findPlaylist(int userID, String playlistName);

    @Query("SELECT * FROM Playlists WHERE userID=:userID")
    LiveData<List<Playlist>> getAllPlaylists(int userID);
}
