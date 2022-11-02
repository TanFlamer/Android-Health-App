package com.example.myapp.fragmentsSport;

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
import com.example.myapp.fragmentsSport.expandableListSport.SportData;
import com.example.myapp.fragmentsSport.expandableListSport.SportExpandableListAdapter;
import com.example.myapp.fragmentsSport.expandableListSport.SportListItem;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link SportList#newInstance} factory method to
 * create an instance of this fragment.
 */
public class SportList extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public SportList() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment SportList.
     */
    // TODO: Rename and change types and number of parameters
    public static SportList newInstance(String param1, String param2) {
        SportList fragment = new SportList();
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
        return inflater.inflate(R.layout.fragment_sport_list, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        List<SportListItem> sportListItemList = new ArrayList<>();

        List<SportData> sportData = new ArrayList<>();
        List<SportData> sportData1 = new ArrayList<>();

        sportData.add(new SportData("lol", 0, 0));
        sportData.add(new SportData("lol", 0, 0));

        sportData1.add(new SportData("lol1", 0, 0));
        sportData1.add(new SportData("lol1", 0, 0));

        sportListItemList.add(new SportListItem("test", sportData));
        sportListItemList.add(new SportListItem("test1", sportData1));

        ExpandableListView expandableListView = requireView().findViewById(R.id.sportExpandableListView);
        SportExpandableListAdapter sportExpandableListAdapter = new SportExpandableListAdapter(getContext(), sportListItemList);
        expandableListView.setAdapter(sportExpandableListAdapter);

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

        expandableListView.setOnChildClickListener(new ExpandableListView.OnChildClickListener() {
            @Override
            public boolean onChildClick(ExpandableListView expandableListView, View view, int i, int i1, long l) {
                String selected = sportExpandableListAdapter.getChild(i, i1).toString();
                Toast.makeText(getContext(), selected, Toast.LENGTH_SHORT).show();
                return true;
            }
        });

        /*ListView listView = requireView().findViewById(R.id.sportListView);

        List<SportData> sportDataList = new ArrayList<>();
        sportDataList.add(new SportData("lol", 0, 0));
        sportDataList.add(new SportData("lol1", 1, 1));

        List<SportListItem> sportListItemList = new ArrayList<>();
        sportListItemList.add(new SportListItem("test", sportDataList));

        SportListAdapter sportListAdapter = new SportListAdapter(getContext(), R.layout.sport_list_item, sportListItemList);
        listView.setAdapter(sportListAdapter);

        listView.setOnItemClickListener((adapterView, view1, i, l) -> {
            SportListItem sportListItem = (SportListItem) listView.getItemAtPosition(i);
            Toast.makeText(getContext(), sportListItem.getDate(), Toast.LENGTH_SHORT).show();
        });*/
    }
}