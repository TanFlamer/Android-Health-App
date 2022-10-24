package com.example.myappv2.subfragments.sports;

import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TableLayout;
import android.widget.TableRow;
import android.widget.TextView;
import android.widget.Toast;

import com.example.myappv2.R;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link SportsList#newInstance} factory method to
 * create an instance of this fragment.
 */
public class SportsList extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public SportsList() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment SportsList.
     */
    // TODO: Rename and change types and number of parameters
    public static SportsList newInstance(String param1, String param2) {
        SportsList fragment = new SportsList();
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
        return inflater.inflate(R.layout.fragment_sports_list, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        addTableRow("test");
        addTableRow("test2");

        TableLayout table = requireView().findViewById(R.id.sportsTable);
        for(int i = 0, j = table.getChildCount(); i < j; i++) {
            View tableRow = table.getChildAt(i);
            if (tableRow instanceof TableRow) {
                // then, you can remove the the row you want...
                // for instance...
                Toast.makeText(getContext(), "test", Toast.LENGTH_SHORT).show();
                TableRow row = (TableRow) tableRow;
                if(((TextView) row.getChildAt(0)).getText().toString().equals("test")) {
                    //table.removeView(row);
                    table.removeViewAt(i);
                }
            }
        }
    }

    public void addTableRow(String text){
        TableLayout tableLayout = requireView().findViewById(R.id.sportsTable);
        TableRow tableRow = new TableRow(getContext());

        TextView tv0 = new TextView(getContext());
        tv0.setText(text);
        tableRow.addView(tv0);

        TextView tv1 = new TextView(getContext());
        tv1.setText(text);
        tableRow.addView(tv1);

        TextView tv2 = new TextView(getContext());
        tv2.setText(text);
        tableRow.addView(tv2);

        TextView tv3 = new TextView(getContext());
        tv3.setText(text);
        tableRow.addView(tv3);

        tableLayout.addView(tableRow);
    }
}