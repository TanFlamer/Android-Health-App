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
import com.example.myapp.ViewHolder;
import com.example.myapp.databaseFiles.playlist.Playlist;
import com.example.myapp.databaseFiles.song.Song;

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
            //create new view holder
            ViewHolder viewHolder = new ViewHolder();
            //add text view to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.musicPlaylistName));
            //add layout to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.layoutHidden));
            //add button to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.clickEdit));
            //add button to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.clickDelete));
            //set tag to view
            currentItemView.setTag(viewHolder);
        }

        //get view holder
        ViewHolder viewHolder = (ViewHolder) currentItemView.getTag();
        //update playlist view data
        initialiseGroupView(viewHolder, i);
        //return playlist view
        return currentItemView;
    }

    //update playlist view data
    public void initialiseGroupView(ViewHolder viewHolder, int position){
        Playlist playlist = playlistList.get(position);
        //update playlist name
        initialiseGroupName(viewHolder, playlist);
        //update playlist hidden layout
        initialiseHiddenLayout(viewHolder, playlist);
        //update playlist edit button
        initialiseEditButton(viewHolder, playlist);
        //update playlist delete button
        initialiseDeleteButton(viewHolder, playlist);
    }

    //update playlist name
    public void initialiseGroupName(ViewHolder viewHolder, Playlist playlist){
        //get text view ID for playlist name
        TextView nameView = (TextView) viewHolder.getView(R.id.musicPlaylistName);
        //set playlist name
        nameView.setText(playlist.getPlaylistName());
    }

    //update playlist hidden layout
    public void initialiseHiddenLayout(ViewHolder viewHolder, Playlist playlist){
        //get hidden layout by ID
        LinearLayout layoutHidden = (LinearLayout) viewHolder.getView(R.id.layoutHidden);
        //change visibility of hidden layout on long click
        layoutHidden.setVisibility(Boolean.TRUE.equals(buttonMap.get(playlist)) ? View.VISIBLE : View.GONE);
    }

    //update playlist edit button
    public void initialiseEditButton(ViewHolder viewHolder, Playlist playlist){
        //get edit button by ID
        ImageView clickEdit = (ImageView) viewHolder.getView(R.id.clickEdit);
        //send to edit playlist activity on click
        clickEdit.setOnClickListener(v -> context.startActivity(musicPlaylistsViewModel.editPlaylist(playlist.getPlaylistName())));
    }

    //update playlist delete button
    public void initialiseDeleteButton(ViewHolder viewHolder, Playlist playlist){
        //get delete button by ID
        ImageView clickDelete = (ImageView) viewHolder.getView(R.id.clickDelete);
        //show dialog to validate playlist deletion on click
        clickDelete.setOnClickListener(view1 -> musicPlaylistsViewModel.deletePlaylist(context, playlist).show());
    }

    //show or hide hidden layout on long click
    public void onLongClick(int position){
        //get playlist on long click position
        Playlist playlist = playlistList.get(position);
        //invert hidden layout visibility
        buttonMap.put(playlist, Boolean.FALSE.equals(buttonMap.get(playlist)));
        //notify adapter dataset changed
        notifyDataSetChanged();
    }

    @SuppressLint({"InflateParams", "SetTextI18n"})
    @Override //get view for each song
    public View getChildView(int i, int i1, boolean b, View view, ViewGroup viewGroup) {
        View currentItemView = view;

        //inflate new view for song if null
        if(currentItemView == null) {
            currentItemView = LayoutInflater.from(context).inflate(R.layout.music_expandable_list_item_data, null);
            //create new view holder
            ViewHolder viewHolder = new ViewHolder();
            //add text view to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.musicSongName));
            //add text view to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.musicSongLength));
            //set tag to view
            currentItemView.setTag(viewHolder);
        }

        //update song view data
        updateChildView(currentItemView, i, i1);
        //return song view
        return currentItemView;
    }

    //update song view data
    public void updateChildView(View view, int parent, int child){
        //get view holder
        ViewHolder viewHolder = (ViewHolder) view.getTag();
        //Get song at current position
        Song song = Objects.requireNonNull(songPlaylists.get(playlistList.get(parent))).get(child);
        //update song name
        updateChildName(viewHolder, song);
        //update song duration
        updateChildLength(viewHolder, song);
        //set song on click listener
        updateOnClickView(view, parent, child);
    }

    //update song name
    public void updateChildName(ViewHolder viewHolder, Song song){
        //get text view ID for song name
        TextView nameView = (TextView) viewHolder.getView(R.id.musicSongName);
        //set song name
        nameView.setText(song.getSongName());
    }

    //update song duration
    @SuppressLint("DefaultLocale")
    public void updateChildLength(ViewHolder viewHolder, Song song){
        //get text view ID for song duration
        TextView lengthView = (TextView) viewHolder.getView(R.id.musicSongLength);
        //get song length
        int length = song.getSongDuration();
        //set song length
        lengthView.setText(String.format("%d:%02d", length/60, length%60));
    }

    //set song on click listener
    public void updateOnClickView(View view, int parent, int child){
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
