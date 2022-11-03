package com.example.myapp.fragmentsMusic;

import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ExpandableListView;
import android.widget.Toast;

import com.example.myapp.R;
import com.example.myapp.fragmentsMusic.expandableListMusic.MusicExpandableListAdapter;
import com.example.myapp.fragmentsMusic.expandableListMusic.MusicExpandableListData;
import com.example.myapp.fragmentsMusic.expandableListMusic.MusicExpandableListItem;
import com.example.myapp.fragmentsSport.expandableListSport.SportExpandableListAdapter;
import com.example.myapp.fragmentsSport.expandableListSport.SportExpandableListData;
import com.example.myapp.fragmentsSport.expandableListSport.SportExpandableListItem;

import java.util.ArrayList;
import java.util.List;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link MusicPlaylists#newInstance} factory method to
 * create an instance of this fragment.
 */
public class MusicPlaylists extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public MusicPlaylists() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment Playlists.
     */
    // TODO: Rename and change types and number of parameters
    public static MusicPlaylists newInstance(String param1, String param2) {
        MusicPlaylists fragment = new MusicPlaylists();
        Bundle args = new Bundle();
        args.putString(ARG_PARAM1, param1);
        args.putString(ARG_PARAM2, param2);
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() != null) {
            mParam1 = getArguments().getString(ARG_PARAM1);
            mParam2 = getArguments().getString(ARG_PARAM2);
        }
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_music_playlists, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        List<MusicExpandableListItem> musicExpandableListItemList = new ArrayList<>();

        List<MusicExpandableListData> musicExpandableListDataList = new ArrayList<>();
        List<MusicExpandableListData> musicExpandableListDataList1 = new ArrayList<>();

        musicExpandableListDataList.add(new MusicExpandableListData("lol", 0));
        musicExpandableListDataList.add(new MusicExpandableListData("lol", 0));

        musicExpandableListDataList1.add(new MusicExpandableListData("lol1", 0));
        musicExpandableListDataList1.add(new MusicExpandableListData("lol1", 0));

        musicExpandableListItemList.add(new MusicExpandableListItem("test", musicExpandableListDataList));
        musicExpandableListItemList.add(new MusicExpandableListItem("test1", musicExpandableListDataList1));

        ExpandableListView expandableListView = requireView().findViewById(R.id.musicExpandableListView);
        MusicExpandableListAdapter musicExpandableListAdapter = new MusicExpandableListAdapter(getContext(), musicExpandableListItemList);
        expandableListView.setAdapter(musicExpandableListAdapter);

        expandableListView.setOnGroupExpandListener(new ExpandableListView.OnGroupExpandListener() {
            int lastExpandedPosition = -1;
            @Override
            public void onGroupExpand(int i) {
                if(lastExpandedPosition != -1 && i != lastExpandedPosition){
                    expandableListView.collapseGroup(lastExpandedPosition);
                }
                lastExpandedPosition = i;
            }
        });

        expandableListView.setOnChildClickListener((expandableListView1, view1, i, i1, l) -> {
            String selected = musicExpandableListAdapter.getChild(i, i1).toString();
            Toast.makeText(getContext(), selected, Toast.LENGTH_SHORT).show();
            return true;
        });
    }
}