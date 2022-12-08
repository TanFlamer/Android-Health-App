package com.example.myapp.fragments.music.musicPlaylists;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.example.myapp.MusicPlayer;
import com.example.myapp.R;
import com.example.myapp.databasefiles.playlist.Playlist;
import com.example.myapp.databasefiles.song.Song;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class MusicPlaylistsAdapter extends BaseExpandableListAdapter {

    private final Context context;
    private final List<Playlist> playlistList;
    private final HashMap<Playlist, List<Song>> songPlaylists;
    private final HashMap<Playlist, Boolean> buttonMap;
    private final MusicPlaylistsViewModel musicPlaylistsViewModel;

    //constructor for playlist list adapter
    public MusicPlaylistsAdapter(Context context, HashMap<Playlist, List<Song>> songPlaylists, MusicPlaylistsViewModel musicPlaylistsViewModel){
        this.context = context;
        this.playlistList = new ArrayList<>(songPlaylists.keySet());
        this.songPlaylists = songPlaylists;
        this.musicPlaylistsViewModel = musicPlaylistsViewModel;
        buttonMap = new HashMap<>();
        for (Playlist playlist : playlistList) buttonMap.put(playlist, false);
    }

    @Override //get number of playlist
    public int getGroupCount() {
        return playlistList.size();
    }

    @Override //get song count of playlist
    public int getChildrenCount(int i) {
        return Objects.requireNonNull(songPlaylists.get(playlistList.get(i))).size();
    }

    @Override //get playlist
    public Object getGroup(int i) {
        return songPlaylists.get(playlistList.get(i));
    }

    @Override //get song from playlist
    public Object getChild(int i, int i1) {
        return Objects.requireNonNull(songPlaylists.get(playlistList.get(i))).get(i1);
    }

    @Override //get playlist ID
    public long getGroupId(int i) {
        return i;
    }

    @Override //get song ID
    public long getChildId(int i, int i1) {
        return i1;
    }

    @Override //check if ID stable
    public boolean hasStableIds() {
        return true;
    }

    @SuppressLint("InflateParams")
    @Override //get view for each playlist
    public View getGroupView(int i, boolean b, View view, ViewGroup viewGroup) {
        View currentItemView = view;

        //inflate new view for playlist if null
        if(currentItemView == null) {
            currentItemView = LayoutInflater.from(context).inflate(R.layout.music_expandable_list_item, null);
        }

        //initialise playlist view data
        initialiseGroupView(currentItemView, i);
        //return playlist view
        return currentItemView;
    }

    //initialise playlist view data
    public void initialiseGroupView(View view, int position){
        Playlist playlist = playlistList.get(position);
        //initialise playlist name
        initialiseGroupName(view, playlist);
        //initialise playlist hidden layout
        initialiseHiddenLayout(view, playlist);
        //initialise playlist edit button
        initialiseEditButton(view, playlist);
        //initialise playlist delete button
        initialiseDeleteButton(view, playlist);
    }

    //initialise playlist name
    public void initialiseGroupName(View view, Playlist playlist){
        //get text view ID for playlist name
        TextView nameView = view.findViewById(R.id.musicPlaylistName);
        //set playlist name
        nameView.setText(playlist.getPlaylistName());
    }

    //show or hide hidden layout on long click
    public void onLongClick(int position){
        //get long click position
        Playlist playlist = playlistList.get(position);
        //invert hidden layout visibility
        buttonMap.put(playlist, Boolean.FALSE.equals(buttonMap.get(playlist)));
        //notify adapter dataset changed
        notifyDataSetChanged();
    }

    //initialise playlist hidden layout
    public void initialiseHiddenLayout(View view, Playlist playlist){
        //get hidden layout by ID
        LinearLayout layoutHidden = view.findViewById(R.id.layoutHidden);
        //change visibility of hidden layout on long click
        layoutHidden.setVisibility(Boolean.TRUE.equals(buttonMap.get(playlist)) ? View.VISIBLE : View.GONE);
    }

    //initialise playlist edit button
    public void initialiseEditButton(View view, Playlist playlist){
        //get edit button by ID
        ImageView clickEdit = view.findViewById(R.id.clickEdit);
        //send to edit playlist on click
        clickEdit.setOnClickListener(v -> context.startActivity(musicPlaylistsViewModel.editPlaylist(playlist.getPlaylistName())));
    }

    //initialise playlist delete button
    public void initialiseDeleteButton(View view, Playlist playlist){
        //get delete button by ID
        ImageView clickDelete = view.findViewById(R.id.clickDelete);
        //show delete dialog on click
        clickDelete.setOnClickListener(view1 -> musicPlaylistsViewModel.deletePlaylist(context, playlist).show());
    }

    @SuppressLint({"InflateParams", "SetTextI18n"})
    @Override //get view for each song
    public View getChildView(int i, int i1, boolean b, View view, ViewGroup viewGroup) {
        View currentItemView = view;

        //inflate new view for song if null
        if(currentItemView == null) {
            currentItemView = LayoutInflater.from(context).inflate(R.layout.music_expandable_list_item_data, null);
        }

        //initialise song view data
        initialiseChildView(currentItemView, i, i1);
        //return song view
        return currentItemView;
    }

    //initialise song view data
    public void initialiseChildView(View view, int parent, int child){
        Song song = Objects.requireNonNull(songPlaylists.get(playlistList.get(parent))).get(child);
        //get song name
        initialiseChildName(view, song);
        //get song duration
        initialiseChildLength(view, song);
        //set song on click listener
        initialiseOnClickView(view, parent, child);
    }

    //get song name
    public void initialiseChildName(View view, Song song){
        //get text view ID for song name
        TextView nameView = view.findViewById(R.id.musicSongName);
        //set song name
        nameView.setText(song.getSongName());
    }

    //get song duration
    @SuppressLint("DefaultLocale")
    public void initialiseChildLength(View view, Song song){
        //get text view ID for song duration
        TextView lengthView = view.findViewById(R.id.musicSongLength);
        int length = song.getSongDuration();
        //set song length
        lengthView.setText(String.format("%d:%02d", length/60, length%60));
    }

    //set song on click listener
    public void initialiseOnClickView(View view, int parent, int child){
        //get music player
        MusicPlayer musicPlayer = musicPlaylistsViewModel.getMusicPlayer();
        //load clicked playlist and song in music player
        view.setOnClickListener(v -> musicPlayer.setPlaylist(songPlaylists.get(playlistList.get(parent)), child));
    }

    @Override //check if song selectable
    public boolean isChildSelectable(int i, int i1) {
        return true;
    }

    //update playlist list when playlist or song list changes
    public void updateMusicPlaylists(HashMap<Playlist, List<Song>> newSongPlaylists, String data, String order){
        //clear old playlists
        playlistList.clear();
        //add new playlists
        playlistList.addAll(newSongPlaylists.keySet());
        //clear old song lists
        songPlaylists.clear();
        //add new song lists
        songPlaylists.putAll(newSongPlaylists);
        //sort playlists and song lists
        sortMusicPlaylists(data, order);
    }

    //sort playlists and song lists
    public void sortMusicPlaylists(String data, String order){
        //sort playlists and song lists
        musicPlaylistsViewModel.sortPlaylists(playlistList, songPlaylists, data, order);
        //hide hidden layout for all playlists
        for(Playlist playlist : playlistList) buttonMap.put(playlist, false);
        //notify adapter dataset changed
        notifyDataSetChanged();
    }
}
